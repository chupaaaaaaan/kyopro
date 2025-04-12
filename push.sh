#!/bin/bash

set -euo pipefail
cd $(dirname $0)

if [ ! -f url ]; then
    echo "Cannot find the file: url"
    exit 1
fi

URL=$(cat url)
MAINHS=submission/Main.hs
BUNDLEDHS=submission/Bundled.hs

if [ ! -f "${MAINHS}" ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

cabal run bundler "Judge" "${MAINHS}" "${BUNDLEDHS}"

oj s "${URL}" "${BUNDLEDHS}"
