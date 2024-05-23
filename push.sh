#!/bin/bash

set -eu
cd $(dirname $0)

if [ ! -f url ]; then
    echo "Cannot find the file: url"
    exit 1
fi

URL=$(cat url)
CODEPATH=${URL##*://}
MAINHS=src/${CODEPATH}/Main.hs

if [ ! -f ${MAINHS} ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

cp -f ${MAINHS} .

oj s ${URL} Main.hs

rm -f Main.hs
