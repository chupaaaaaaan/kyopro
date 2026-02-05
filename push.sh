#!/bin/bash

set -euo pipefail
cd $(dirname $0)

if [ ! -f url ]; then
    echo "Cannot find the file: url"
    exit 1
fi

URL=$(cat url)
BUNDLEDHS=submission/Bundled.hs

# check
./check.sh

# build
./build.sh -j

# submit
oj s "${URL}" "${BUNDLEDHS}"
