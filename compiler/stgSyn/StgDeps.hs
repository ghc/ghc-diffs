-- | Dependency analysis on STG terms.
module StgDeps
  ( annTopBindingsDeps
  , FVS
  , depSortStgBinds
  ) where

import GhcPrelude

import Id (idName)
import Module (Module)
import Name (Name, nameIsLocalOrFrom)
import NameEnv (depAnal)
import Outputable
import StgSyn
import Var
import VarSet

import Data.Graph (SCC (..))

-- | Set of bound variables
type BVS = DVarSet

-- | Set of free variables
type FVS = DVarSet

-- | Annotate top-level STG bindings with dependencies in this module.
-- Dependencies are free variables in a binding, but we only consider
-- dependencies in the current module.
annTopBindingsDeps :: Module -> [StgTopBinding] -> [(StgTopBinding, FVS)]
annTopBindingsDeps mod bs = zip bs (map top_bind bs)
  where
    top_bind :: StgTopBinding -> FVS

    top_bind StgTopStringLit{} =
      emptyDVarSet

    top_bind (StgTopLifted bs) =
      binding emptyDVarSet bs

    binding :: BVS -> StgBinding -> FVS

    binding bounds (StgNonRec _ r) =
      rhs bounds r

    binding bounds (StgRec bndrs) =
      unionDVarSets $
        map (bind_non_rec (extendDVarSetList bounds (map fst bndrs))) bndrs

    bind_non_rec :: BVS -> (Id, StgRhs) -> FVS
    bind_non_rec bounds (_, r) =
        rhs bounds r

    rhs :: BVS -> StgRhs -> FVS

    rhs bounds (StgRhsClosure _ _ _ as e) =
      expr (extendDVarSetList bounds as) e

    rhs bounds (StgRhsCon _ _ as) =
      args bounds as

    var :: BVS -> Var -> FVS
    var bounds v
      | nameIsLocalOrFrom mod (idName v) && not (elemDVarSet v bounds)
      = unitDVarSet v
      | otherwise
      = emptyDVarSet

    arg :: BVS -> StgArg -> FVS
    arg bounds (StgVarArg v) = var bounds v
    arg _ StgLitArg{}        = emptyDVarSet

    args :: BVS -> [StgArg] -> FVS
    args bounds as =
      unionDVarSets (map (arg bounds) as)

    expr :: BVS -> StgExpr -> FVS

    expr bounds (StgApp f as) =
      var bounds f `unionDVarSet` args bounds as

    expr _ StgLit{} =
      emptyDVarSet

    expr bounds (StgConApp _ as _) =
      args bounds as

    expr bounds (StgOpApp _ as _) =
      args bounds as

    expr _ lam@StgLam{} =
      pprPanic "annTopBindingsDeps" (text "Found lambda:" $$ ppr lam)

    expr bounds (StgCase scrut scrut_bndr _ as) =
      expr bounds scrut `unionDVarSet`
        alts (extendDVarSet bounds scrut_bndr) as

    expr bounds (StgLet _ bs e) =
      binding bounds bs `unionDVarSet`
        expr (extendDVarSetList bounds (bindersOf bs)) e

    expr bounds (StgLetNoEscape _ bs e) =
      binding bounds bs `unionDVarSet`
        expr (extendDVarSetList bounds (bindersOf bs)) e

    expr bounds (StgTick _ e) =
      expr bounds e

    alts :: BVS -> [StgAlt] -> FVS
    alts bounds = unionDVarSets . map (alt bounds)

    alt :: BVS -> StgAlt -> FVS
    alt bounds (_, bndrs, e) =
      expr (extendDVarSetList bounds bndrs) e

depSortStgBinds :: [(StgTopBinding, FVS)] -> [(StgTopBinding, FVS)]
depSortStgBinds = map get_binds . depAnal defs uses
  where
    uses, defs :: (StgTopBinding, FVS) -> [Name]

    -- TODO (osa): I'm unhappy about two things in this code:
    --
    --     * Why do we need Name instead of Id for uses and dependencies?
    --     * Why do we need a [Name] instead of `Set Name`? Surely depAnal
    --       doesn't need any ordering.

    uses (StgTopStringLit{}, _) = []
    uses (StgTopLifted{}, fvs) = map idName (dVarSetElems fvs)

    defs (bind, _) = map idName (bindersOfTop bind)

    get_binds (AcyclicSCC bind) = bind
    get_binds (CyclicSCC binds) = pprPanic "depSortStgBinds" (text "Found cyclic SCC:" $$ ppr binds)
