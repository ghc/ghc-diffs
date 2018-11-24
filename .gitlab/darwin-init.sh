#!/bin/bash

set -e

root=`pwd`
ghc_bin="`pwd`/ghc-$GHC_VERSION/bin"
PATH="$ghc_bin:$PATH"

if [ -d "`pwd`/cabal-cache" ]; then
    cp -Rf cabal-cache $APPDATA/cabal
fi

if [ ! -d "`pwd`/ghc-$GHC_VERSION" ]; then
    mkdir -p tmp
    cd tmp
    curl https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-apple-darwin.tar.xz | tar -xJ
    cd ghc-$GHC_VERSION
    ./configure --prefix=$root/ghc-$GHC_VERSION
    make install
    cd ../..
    rm -Rf tmp
fi

if [ ! -f $ghc_bin/cabal ]; then
    cabal_tarball="https://www.haskell.org/cabal/release/cabal-install-2.2.0.0/cabal-install-2.2.0.0-x86_64-apple-darwin-sierra.tar.gz"
    curl $cabal_tarball | tar -xz
    mv cabal $ghc_bin
fi

if [ ! -f $ghc_bin/happy ]; then
    cabal update
    cabal install happy --bindir=$ghc_bin
fi

if [ ! -f $ghc_bin/alex ]; then
    cabal update
    cabal install alex --bindir=$ghc_bin
fi

