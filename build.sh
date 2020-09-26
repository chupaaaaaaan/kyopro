#!/bin/bash

cd $(dirname $0)

sourceDir="$1"

ghc -o a.out -O2 ${sourceDir}/Main.hs

rm -f ${sourceDir}/Main.{hi,o}
