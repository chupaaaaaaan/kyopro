#!/bin/bash

cd $(dirname $0)

if [ $# -eq 0 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

sourceDir="$1"

if [ ! -f ${sourceDir}/Main.hs ]; then
    echo "${sourceDir}/Main.hs does not exist"
    exit 1
fi

ghc -package-env ./package.local -o a.out -O2 ${sourceDir}/Main.hs

rm -f ${sourceDir}/Main.{hi,o}
