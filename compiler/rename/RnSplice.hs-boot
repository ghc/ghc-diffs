module RnSplice where

import GhcPrelude
import HsSyn
import TcRnMonad
import NameSet
import Outputable


rnSpliceType :: HsSplice GhcPs   -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsSplice GhcPs   -> RnM ( Either (Pat GhcPs) (Pat GhcRn)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)

traceSplice :: SpliceInfo -> TcM ()

data SpliceInfo
  = SpliceInfo
    { spliceDescription  :: String
    , spliceSource       :: Maybe (LHsExpr GhcRn)
    , spliceIsDecl       :: Bool
    , spliceGenerated    :: SDoc
    }
