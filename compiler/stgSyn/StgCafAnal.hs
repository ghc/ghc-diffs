module StgCafAnal (stgCafAnal) where

import GhcPrelude

import Id
import IdInfo
import Name (Name)
import NameEnv (depAnal)
-- import Outputable
import StgDeps
import StgSyn
import VarEnv
import VarSet

import Data.Graph (SCC (..))

-- TODO: Only recording Ids in VarEnv just to be able to get useful debug
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
-- The `Id` in the range of the map is to help debugging (VarEnv only holds
-- Uniques of keys).
--
stgCafAnal :: [StgTopBinding] -> VarEnv (Id, CafInfo)
stgCafAnal pgm = foldr update_env emptyVarEnv pgm_sorted
  where
    -- Program with dependency annotations
    pgm_deps = annTopBindingsDeps pgm
    -- Sort in dependency order (defs come before uses)
    pgm_sorted = depSort pgm_deps
    -- Do CAF analysis for the binding, update the environment
    update_env bind env = plusVarEnv env (cafAnalTopBinding env bind)

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
cafAnalTopBinding :: VarEnv (Id, CafInfo) -> (StgTopBinding, FVs) -> VarEnv (Id, CafInfo)

cafAnalTopBinding _ (StgTopStringLit bndr _, _)
  = unitVarEnv bndr (bndr, NoCafRefs)

cafAnalTopBinding env (StgTopLifted (StgNonRec bndr rhs), fvs)
  = unitVarEnv bndr (bndr, caf_info)
  where
    caf_info
      | fvsHaveCafRefs env fvs || rhsIsCaf rhs
      = MayHaveCafRefs
      | otherwise
      = NoCafRefs

cafAnalTopBinding env (StgTopLifted bndrs_@(StgRec bndrs), fvs)
  = mkVarEnv (map (\b -> (b, (b, caf_info))) (bindersOf bndrs_))
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

fvsHaveCafRefs :: VarEnv (Id, CafInfo) -> FVs -> Bool
fvsHaveCafRefs env fvs = any (fvHasCafRefs env) (dVarSetElems fvs)

fvHasCafRefs :: VarEnv (Id, CafInfo) -> Var -> Bool
fvHasCafRefs env v = caf_info == MayHaveCafRefs
  where
    caf_info = case lookupVarEnv env v of
      Nothing     -> idCafInfo v -- TODO check that this is really an imported name and not a bug!
      Just (_, c) -> c
