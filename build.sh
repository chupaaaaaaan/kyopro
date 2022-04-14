#!/bin/bash

cd $(dirname $0)

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

URL=$1
CODEPATH=${URL##*://}
MAINHS=${CODEPATH}/Main.hs

if [ ! -f ${MAINHS} ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

ghc -package-env ./package.local -o a.out -O2 ${MAINHS}

rm -f ${CODEPATH}/Main.{o,hi}
