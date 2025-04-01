#!/bin/bash

set -euo pipefail
cd $(dirname $0)

MAINHS=submission/Main.hs

if [ ! -f ${MAINHS} ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

# build
cabal build lib:kyopro --enable-profiling
cabal build exe:submission --enable-profiling

cp $(find dist-newstyle/build -type f -name submission | head -n1) submission/a.out

# test
oj t -d cases/sample -c submission/a.out
