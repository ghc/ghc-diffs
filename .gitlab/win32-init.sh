#!/bin/bash -e

ghc_bin="`pwd`/ghc-$GHC_VERSION/bin"
PATH="$ghc_bin:$PATH"

if [ -d "`pwd`/cabal-cache" ]; then
    cp -Rf cabal-cache $APPDATA/cabal
fi

if [ ! -d "`pwd`/ghc-$GHC_VERSION" ]; then
    curl https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-unknown-mingw32.tar.xz | tar -xJ
fi

if [ ! -f $ghc_bin/cabal ]; then
    curl https://www.haskell.org/cabal/release/cabal-install-2.2.0.0/cabal-install-2.2.0.0-i386-unknown-mingw32.zip > /tmp/cabal.zip
    unzip /tmp/cabal.zip
    mv cabal.exe $ghc_bin
fi

if [ ! -f $ghc_bin/happy ]; then
    cabal update
    cabal install happy --bindir=$ghc_bin
fi

if [ ! -f $ghc_bin/alex ]; then
    cabal update
    cabal install alex --bindir=$ghc_bin
fi

