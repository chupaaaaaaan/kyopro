#!/bin/bash

set -euo pipefail
cd $(dirname $0)

# parse options
OPTF=0

while getopts f option 2> /dev/null
do
    case ${option} in
        f) OPTF=1 ;;
        \?)
            echo "Only -f can be specified." 1>&2
            exit 1
            ;;
    esac
done

if [ ! -f url ]; then
    echo "Cannot find the file: url"
    exit 1
fi

URL=$(cat url)
BUNDLEDHS=submission/Bundled.hs

# build
./build.sh -j

# test
if ! oj t -d cases/sample -c submission/a.out; then
    if [ "$OPTF" -eq 0 ]; then
        exit 1
    fi

    echo "Tests failed. Proceeding with submission because -f was specified."
fi

# submit
oj s "${URL}" "${BUNDLEDHS}"
