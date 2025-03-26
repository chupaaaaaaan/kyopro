#!/bin/bash

cd $(dirname $0)

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

URL=$1
CODEPATH=${URL##*://}

mkdir -p code/${CODEPATH}
echo ${URL} > url

if [ ! -f "code/${CODEPATH}/Main.hs" ]; then
    cat src/template.hs > code/${CODEPATH}/Main.hs
fi

rm -fr cases/sample/*
oj d -d cases/sample ${URL} 
