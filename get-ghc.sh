#!/bin/sh
# script for installing GHC on Linux

prefix=/usr/local
arch=x86_64

ghc_ver=7.8.4
#ghc_dist=unknown-linux-centos65 # requires GMP4 (libgmp.so.3)
ghc_dist=unknown-linux-deb7 # requires GMP5 (libgmp.so.10)

cabal_ver=1.22.0.0
cabal_dist=unknown-linux

# ----------------------------------------------------------------------------
set -e

# download a file from the given URL
#
# inputs:
#   - 1:            URL
#
# output:
#   - /dev/stdout:  data of downloaded file
#
download_tool=
download() {
    if [ $# -ne 1 ]
    then
        echo >&2 "download: expected one argument"
        return 1
    fi

    # figure out which tool we should use to download files
    [ "$download_tool" ] || {
        if command >/dev/null 2>&1 -v curl
        then
            download_tool=curl
        elif command >/dev/null 2>&1 -v wget
        then
            download_tool=wget
        else
            download_tool=none
        fi
    }

    # call the download tool
    case $download_tool in
        curl) curl -fsLS    -- "$1";;
        wget) wget -nv -O - -- "$1";;
        *)    echo >&2 "download: either `curl` or `wget` must be installed"
              return 1;;
    esac
}

url=http://haskell.org/ghc/dist
url=$url/$ghc_ver/ghc-$ghc_ver-$arch-$ghc_dist.tar.xz
download "$url" | tar xJf -
(
    cd "ghc-$ghc_ver"
    ./configure --prefix="$prefix"
    make install
)

url=http://haskell.org/cabal/release
url=$url/cabal-install-$cabal_ver/cabal-$arch-$cabal_dist.tar.gz
(
    cd "$prefix/bin"
    download "$url" | tar xzf -
)
