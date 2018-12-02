{-# LANGUAGE TupleSections #-}

module StgCafAnal (stgCafAnal) where

import GhcPrelude

import Id
import IdInfo
import StgDeps (FVS)
import StgSyn
import VarEnv
import VarSet

-- | Given an STG program (annotated with dependencies, see StgDeps) return a
-- mapping from ids to their CafInfos.
--
-- Note that CAFs are considered as MayHaveCafRefs.
--
stgCafAnal :: [(StgTopBinding, FVS)] -> VarEnv CafInfo
stgCafAnal = foldr f emptyVarEnv
  where
    f :: (StgTopBinding, FVS) -> VarEnv CafInfo -> VarEnv CafInfo
    f bind env = plusVarEnv env (cafAnalTopBinding env bind)

-- | Given CafInfos of dependencies and a top-level binding return CafInfos of
-- Ids that the binding binds.
cafAnalTopBinding :: VarEnv CafInfo -> (StgTopBinding, FVS) -> VarEnv CafInfo

cafAnalTopBinding _ (StgTopStringLit{}, _) = emptyVarEnv

cafAnalTopBinding env (StgTopLifted (StgNonRec bndr rhs), fvs)
  = unitVarEnv bndr caf_info
  where
    caf_info
      | fvsHaveCafRefs env fvs || topRhsHasCafRefs rhs
      = MayHaveCafRefs
      | otherwise
      = NoCafRefs

cafAnalTopBinding env (StgTopLifted bndrs_@(StgRec bndrs), fvs)
  = mkVarEnv (map (, caf_info) (bindersOf bndrs_))
  where
    caf_info
      | fvsHaveCafRefs env fvs || any (topRhsHasCafRefs . snd) bndrs
      = MayHaveCafRefs
      | otherwise
      = NoCafRefs

fvsHaveCafRefs :: VarEnv CafInfo -> FVS -> Bool
fvsHaveCafRefs env fvs = any (fvHasCafRefs env) (dVarSetElems fvs)

fvHasCafRefs :: VarEnv CafInfo -> Var -> Bool
fvHasCafRefs env v = caf_info == MayHaveCafRefs
  where
    caf_info = case lookupVarEnv env v of
      Nothing -> idCafInfo v -- TODO check that this is really an imported name and not a bug!
      Just c  -> c
