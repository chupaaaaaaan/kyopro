#!/bin/bash

set -euo pipefail
cd $(dirname $0)

MAINHS=submission/Main.hs
BUNDLEDHS=submission/Bundled.hs

if [ ! -f "${MAINHS}" ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

# build
cabal run bundler "Local" "${MAINHS}" "${BUNDLEDHS}"
ghc -package-env .package.local -o submission/a.out -O2 -prof -fprof-auto "${BUNDLEDHS}"

# test
oj t -d cases/sample -c submission/a.out
