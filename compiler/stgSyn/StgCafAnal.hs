{-
Note [CafInfo analysis]
~~~~~~~~~~~~~~~~~~~~~~~

Some history: before this pass CafInfo of bindings were computed in TidyPgm.
After that any later passes (CorePrep, CoreToStg, UnariseStg, StgCse, ...) had
to preserve this information otherwise the CafInfos written to the interface
files would be inconsistent with the actual CafInfos (this caused #15038). To
avoid this we had a lot of code in CorePrep (to avoid floating things out and
invalidating computed CafInfos by making more things CAFFY), and we had sanity
checks in CoreToStg.

With this pass we now do the analysis at a stage where we know the CafInfos will
certainly not change: right before Cmm generation (we don't do transformations
in Cmm that change CafInfos). With this we no longer have to preserve CafInfos
in Core or STG passes, and lots of hacks, notes and sanity checks are gone. The
cost is that in non-batch mode (when building only one module) we keep the
interface in memory longer than before.

See comments around stgCafAnal below for an overview of the new pass.
-}

module StgCafAnal (stgCafAnal) where

import GhcPrelude

import Id
import IdInfo
import Name (Name)
import NameEnv
import Outputable
import StgSyn
import VarSet

import Data.Graph (SCC (..))

--------------------------------------------------------------------------------
-- * Dependency analysis

-- | Set of bound variables
type BVs = DVarSet

-- | Set of free variables
type FVs = DVarSet

-- | Dependency analysis on STG terms.
--
-- Dependencies of a binding are just free variables in the binding. This
-- includes imported ids and ids in the current module. For recursive groups we
-- just return one set of free variables which is just the union of dependencies
-- of all bindings in the group.
--
-- Implementation: pass bound variables (BVs) to recursive calls, get free
-- variables (FVs) back.
--
annTopBindingsDeps :: [StgTopBinding] -> [(StgTopBinding, FVs)]
annTopBindingsDeps bs = zip bs (map top_bind bs)
  where
    top_bind :: StgTopBinding -> FVs

    top_bind StgTopStringLit{} =
      emptyDVarSet

    top_bind (StgTopLifted bs) =
      binding emptyDVarSet bs

    binding :: BVs -> StgBinding -> FVs

    binding bounds (StgNonRec _ r) =
      rhs bounds r

    binding bounds (StgRec bndrs) =
      unionDVarSets $
        map (bind_non_rec (extendDVarSetList bounds (map fst bndrs))) bndrs

    bind_non_rec :: BVs -> (Id, StgRhs) -> FVs
    bind_non_rec bounds (_, r) =
        rhs bounds r

    rhs :: BVs -> StgRhs -> FVs

    rhs bounds (StgRhsClosure _ _ _ as e) =
      expr (extendDVarSetList bounds as) e

    rhs bounds (StgRhsCon _ _ as) =
      args bounds as

    var :: BVs -> Var -> FVs
    var bounds v
      | not (elemDVarSet v bounds)
      = unitDVarSet v
      | otherwise
      = emptyDVarSet

    arg :: BVs -> StgArg -> FVs
    arg bounds (StgVarArg v) = var bounds v
    arg _ StgLitArg{}        = emptyDVarSet

    args :: BVs -> [StgArg] -> FVs
    args bounds as =
      unionDVarSets (map (arg bounds) as)

    expr :: BVs -> StgExpr -> FVs

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

    alts :: BVs -> [StgAlt] -> FVs
    alts bounds = unionDVarSets . map (alt bounds)

    alt :: BVs -> StgAlt -> FVs
    alt bounds (_, bndrs, e) =
      expr (extendDVarSetList bounds bndrs) e

--------------------------------------------------------------------------------
-- * CAF analysis

-- TODO: Only recording Ids in NameEnv just to be able to get useful debug
-- prints. Maybe only do it in debug build.

-- | Given an STG program return a mapping from binders to their CafInfos. We
-- compute CafInfos in three steps:
--
-- * First do dependency analysis (StgDeps). Dependency analysis is just free
--   variable analysis, but we give it a different name to avoid confusing it
--   with StgFVs. See module documentations of StgFVs and StgDeps for how they
--   differ.
--
-- * Then bring the bindings into dependency order so that definitions come
--   before uses. This pass uses dependencies computed in the previous step, and
--   does not traverse the whole syntax tree. Most of the work here is done by
--   NameEnv.depAnal.
--
-- * Finally do one pass over bindings to give each binder a CafInfo. This pass
--   does not visit expressions in the RHSs. We just look at CafInfos of
--   dependencies, if any of them is CAFFY then the current binder CAFFY (if
--   we're visiting a recursive group, then the whole group is CAFFY). Because
--   bindings are in dependency order, by the time we visit a binding (or a
--   recursive group) we know CafInfo of all its dependencies (for imported
--   dependencies these are recorded in `idCafInfo`, otherwise we refer to the
--   environment), so this pass is trivial.
--
-- The `Id` in the range of the map is to help debugging (NameEnv only holds
-- Uniques of keys).
--
stgCafAnal :: [StgTopBinding] -> NameEnv (Id, CafInfo)
stgCafAnal pgm = foldr update_env emptyNameEnv pgm_sorted
  where
    -- Program with dependency annotations
    pgm_deps = annTopBindingsDeps pgm
    -- Sort in dependency order (defs come before uses)
    pgm_sorted = reverse (depSort pgm_deps)
    -- Do CAF analysis for the binding, update the environment
    update_env bind env = plusNameEnv env (cafAnalTopBinding env bind)

depSort :: [(StgTopBinding, FVs)] -> [(StgTopBinding, FVs)]
depSort = concatMap get_binds . depAnal defs uses
  where
    uses, defs :: (StgTopBinding, FVs) -> [Name]

    -- TODO (osa): I'm unhappy about two things in this code:
    --
    --     * Why do we need Name instead of Id for uses and dependencies?
    --     * Why do we need a [Name] instead of `Set Name`? Surely depAnal
    --       doesn't need any ordering.

    uses (StgTopStringLit{}, _) = []
    uses (StgTopLifted{}, fvs)  = map idName (dVarSetElems fvs)

    defs (bind, _) = map idName (bindersOfTop bind)

    get_binds (AcyclicSCC bind) =
      [bind]
    get_binds (CyclicSCC binds) =
      -- TODO: Disabling this panic for now, see #16038
      -- pprPanic "depSortStgBinds" (text "Found cyclic SCC:" $$ ppr binds)
      binds

-- | Given CafInfos of dependencies and a top-level binding return CafInfos of
-- Ids that the binding binds.
cafAnalTopBinding :: NameEnv (Id, CafInfo) -> (StgTopBinding, FVs) -> NameEnv (Id, CafInfo)

cafAnalTopBinding _ (StgTopStringLit bndr _, _)
  = unitNameEnv (idName bndr) (bndr, NoCafRefs)

cafAnalTopBinding env (StgTopLifted (StgNonRec bndr rhs), fvs)
  = unitNameEnv (idName bndr) (bndr, caf_info)
  where
    caf_info
      | fvsHaveCafRefs env fvs || rhsIsCaf rhs
      = MayHaveCafRefs
      | otherwise
      = NoCafRefs

cafAnalTopBinding env (StgTopLifted bndrs_@(StgRec bndrs), fvs)
  = mkNameEnv (map (\b -> (idName b, (b, caf_info))) (bindersOf bndrs_))
  where
    caf_info
      | fvsHaveCafRefs env fvs || any (rhsIsCaf . snd) bndrs
      = MayHaveCafRefs
      | otherwise
      = NoCafRefs

-- Only called on top-level RHSs
rhsIsCaf :: StgRhs -> Bool
rhsIsCaf (StgRhsClosure _ _ upd args _) = null args && isUpdatable upd
-- Note that all variable arguments of should be recorded as a free variable, so
-- we don't need to consider this case, `fvsHaveCafRefs` check already checks
-- these arguments.
rhsIsCaf StgRhsCon{}                    = False

fvsHaveCafRefs :: NameEnv (Id, CafInfo) -> FVs -> Bool
fvsHaveCafRefs env fvs = any (fvHasCafRefs env) (dVarSetElems fvs)

fvHasCafRefs :: NameEnv (Id, CafInfo) -> Var -> Bool
fvHasCafRefs env v = caf_info == MayHaveCafRefs
  where
    caf_info = case lookupNameEnv env (idName v) of
      Nothing     -> idCafInfo v -- TODO check that this is really an imported name and not a bug!
      Just (_, c) -> c
