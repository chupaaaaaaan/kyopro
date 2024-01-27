#!/bin/bash

set -eu
cd $(dirname $0)

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

URL=$1
CODEPATH=${URL##*://}
MAINHS=src/${CODEPATH}/Main.hs

if [ ! -f ${MAINHS} ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

rm -rf app/* && mkdir -p app

cp ${MAINHS} app/

# build
ghc -package-env package.local -o app/a.out -O2 -prof -fprof-auto app/Main.hs

# test
oj t -c app/a.out
