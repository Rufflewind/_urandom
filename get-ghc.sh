#!/bin/sh
PREFIX="$HOME"
ARCH=x86_64

GHC_VER=7.8.2
GHC_DIST=unknown-linux-centos65
# note: unknown-linux-centos65 requires GMP4 (libgmp.so.3)

CABAL_VER=1.20.0.1
CABAL_DIST=unknown-linux

U=http://haskell.org/ghc/dist
U="$U"/"$GHC_VER"/ghc-"$GHC_VER"-"$ARCH"-"$GHC_DIST".tar.xz
curl -L "$U" | tar xJf -
(
    cd ghc-7.8.2
    ./configure --prefix="$PREFIX"
    make install
)

U=http://haskell.org/cabal/release
U="$U"/cabal-install-"$CABAL_VER"/cabal-"$ARCH"-"$CABAL_DIST".tar.gz
(
    cd "$PREFIX"/bin
    curl -L "$U" | tar xzf -
)
