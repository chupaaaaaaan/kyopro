#!/bin/bash

cd $(dirname $0)

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

sourceFile="$1"
sourceFileBase=${sourceFile%.hs}

if [ ! -f ${sourceFile} ]; then
    echo "${sourceDir}/Main.hs does not exist"
    exit 1
fi

ghc -package-env ./package.local -o a.out -O2 ${sourceFile}

rm -f ${sourceFileBase}.{o,hi}
