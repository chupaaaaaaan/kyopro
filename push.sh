#!/bin/bash

set -eu
cd $(dirname $0)

if [ ! -f url ]; then
    echo "Cannot find the file: url"
    exit 1
fi

URL=$(cat url)
CODEPATH=${URL##*://}
MAINHS=code/${CODEPATH}/Main.hs

if [ ! -f ${MAINHS} ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

oj s ${URL} ${MAINHS}
