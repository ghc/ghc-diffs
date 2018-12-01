-- | Dependency analysis on STG terms.
module StgDeps where

import GhcPrelude

import Id (idName)
import Module (Module)
import Name (nameIsLocalOrFrom)
import Outputable
import StgSyn
import Util
import Var
import VarSet

import Data.Bifunctor (second)

-- * Ideally we only want to annotate top-level bindings with deps, but that's
--   not possible currently.
--
-- * StgLam uses Id instead of BinderP

-- | Set of bound variables
type Bounds = DVarSet

-- | Set of free variables
type FVS = DVarSet

-- | Annotate top-level STG bindings with dependencies in this module.
-- Dependencies are free variables in the binding, except we only consider
-- dependencies in the current module.
annTopBindingsDeps :: Module -> [StgTopBinding] -> [CgStgTopBinding]
annTopBindingsDeps mod = map top_bind
  where
    top_bind :: StgTopBinding -> CgStgTopBinding
    top_bind (StgTopStringLit bndr bs) =
        StgTopStringLit bndr bs
    top_bind (StgTopLifted binds) =
        StgTopLifted (top_binding binds)

    top_binding :: StgBinding -> CgStgBinding
    top_binding (StgNonRec bndr r) =
        StgNonRec bndr (fst (rhs emptyDVarSet r))
    top_binding (StgRec bndrs) =
        StgRec (map (second (fst . rhs bounds)) bndrs)
      where
        bounds = mkDVarSet (map fst bndrs)

    rhs :: Bounds -> StgRhs -> (CgStgRhs, FVS)

    rhs bounds (StgRhsClosure _ ccs upd as e) =
        (StgRhsClosure fvs ccs upd as e', fvs)
      where
        bounds' = extendDVarSetList bounds as
        (e', fvs) = expr bounds' e

    rhs bounds (StgRhsCon ccs con as) =
        (StgRhsCon ccs con as, args bounds as)

    var :: Bounds -> Var -> FVS
    var bounds v
      | nameIsLocalOrFrom mod (idName v) && not (elemDVarSet v bounds)
      = unitDVarSet v
      | otherwise
      = emptyDVarSet

    arg :: Bounds -> StgArg -> FVS
    arg bounds (StgVarArg v) = var bounds v
    arg _ StgLitArg{} = emptyDVarSet

    args :: Bounds -> [StgArg] -> FVS
    args bounds as =
        unionDVarSets (map (arg bounds) as)

    expr :: Bounds -> StgExpr -> (CgStgExpr, FVS)

    expr bounds (StgApp f as) =
        (StgApp f as, fvs)
      where
        f_fvs = var bounds f
        args_fvs = args bounds as
        fvs = unionDVarSet f_fvs args_fvs

    expr _ (StgLit lit) =
        (StgLit lit, emptyDVarSet)

    expr bounds (StgConApp con as tys) =
        (StgConApp con as tys, args bounds as)

    expr bounds (StgOpApp op as ty) =
        (StgOpApp op as ty, args bounds as)

    expr _ lam@StgLam{} =
      pprPanic "annTopBindingsDeps" (text "Found lambda:" $$ ppr lam)

    expr bounds (StgCase scrut scrut_bndr alt_ty as) =
        (StgCase scrut' scrut_bndr alt_ty alts', fvs)
      where
        (scrut', scrut_fvs) = expr bounds scrut
        (alts', alt_fvs) = alts bounds as
        fvs = unionDVarSet scrut_fvs alt_fvs

    expr bounds (StgLet ext bs e) =
        (StgLet ext  bs' e', fvs)
      where
        e_bounds = extendDVarSetList bounds (bindersOf bs)
        (e', e_fvs) = expr e_bounds e
        (bs', bs_fvs) = binds bounds bs
        fvs = unionDVarSet e_fvs bs_fvs

    -- FIXME: Duplicate of StgLet
    expr bounds (StgLetNoEscape ext bs e) =
        (StgLetNoEscape ext  bs' e', fvs)
      where
        e_bounds = extendDVarSetList bounds (bindersOf bs)
        (e', e_fvs) = expr e_bounds e
        (bs', bs_fvs) = binds bounds bs
        fvs = unionDVarSet e_fvs bs_fvs

    expr bounds (StgTick tick e) =
        (StgTick tick e', fvs)
      where
        (e', fvs) = expr bounds e

    binds :: Bounds -> StgBinding -> (CgStgBinding, FVS)

    binds bounds (StgNonRec bndr r) =
        (StgNonRec bndr rhs', fvs)
      where
        (rhs', fvs) = rhs bounds r
    binds bounds (StgRec bndrs) =
        (StgRec bndrs', fvs)
      where
        bounds' = extendDVarSetList bounds (map fst bndrs)
        (bndrs', fvss) = mapAndUnzip (bind bounds') bndrs
        fvs = unionDVarSets fvss

    bind :: Bounds -> (Id, StgRhs) -> ((Id, CgStgRhs), FVS)
    bind bounds (bndr, r) =
        ((bndr, r'), fvs)
      where
        (r', fvs) = rhs bounds r

    alts :: Bounds -> [StgAlt] -> ([CgStgAlt], FVS)
    alts bounds = second unionDVarSets . mapAndUnzip (alt bounds)

    alt :: Bounds -> StgAlt -> (CgStgAlt, FVS)
    alt bounds (con, bndrs, e) =
        ((con, bndrs, e'), fvs)
      where
        bounds' = extendDVarSetList bounds bndrs
        (e', fvs) = expr bounds' e
