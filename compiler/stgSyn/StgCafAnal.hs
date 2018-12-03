{-# LANGUAGE TupleSections #-}

module StgCafAnal (stgCafAnal) where

import GhcPrelude

import Id
import IdInfo
import StgDeps (FVS)
import StgSyn
import VarEnv
import VarSet

-- TODO: Only recording Ids in VarEnv just to be able to get useful debug
-- prints. Maybe only do it in debug build.

-- | Given an STG program (annotated with dependencies, see StgDeps) return a
-- mapping from ids to their CafInfos.
--
-- Note that CAFs are considered as MayHaveCafRefs.
--
stgCafAnal :: [(StgTopBinding, FVS)] -> VarEnv (Id, CafInfo)
stgCafAnal = foldr f emptyVarEnv
  where
    f :: (StgTopBinding, FVS) -> VarEnv (Id, CafInfo) -> VarEnv (Id, CafInfo)
    f bind env = plusVarEnv env (cafAnalTopBinding env bind)

-- | Given CafInfos of dependencies and a top-level binding return CafInfos of
-- Ids that the binding binds.
cafAnalTopBinding :: VarEnv (Id, CafInfo) -> (StgTopBinding, FVS) -> VarEnv (Id, CafInfo)

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
rhsIsCaf StgRhsCon{} = False

fvsHaveCafRefs :: VarEnv (Id, CafInfo) -> FVS -> Bool
fvsHaveCafRefs env fvs = any (fvHasCafRefs env) (dVarSetElems fvs)

fvHasCafRefs :: VarEnv (Id, CafInfo) -> Var -> Bool
fvHasCafRefs env v = caf_info == MayHaveCafRefs
  where
    caf_info = case lookupVarEnv env v of
      Nothing -> idCafInfo v -- TODO check that this is really an imported name and not a bug!
      Just (_, c) -> c
