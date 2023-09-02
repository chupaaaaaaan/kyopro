#!/bin/bash

set -eu
cd $(dirname $0)

PACKAGE_ENV='package.local'
GHC_VERSION='9.4.5'

ghcup set ghc ${GHC_VERSION}

cabal update
cabal build --only-dependencies --write-ghc-environment-files=always --enable-profiling

mv .ghc.environment.x86_64-linux-${GHC_VERSION} ${PACKAGE_ENV}
sed -i "/package-db dist-newstyle/d" ${PACKAGE_ENV}
