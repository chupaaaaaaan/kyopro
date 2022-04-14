#!/bin/bash

cd $(dirname $0)

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

URL=$1
CODEPATH=${URL##*://}

mkdir -p ${CODEPATH}

if [ ! -f "${CODEPATH}/Main.hs" ]; then
    cat template.hs > ${CODEPATH}/Main.hs
fi

rm -fr test/
oj d ${URL} 
