#!/bin/bash

set -euo pipefail
cd $(dirname $0)

MAINHS=submission/Main.hs
BUNDLEDHS=submission/Bundled.hs

if [ ! -f "${MAINHS}" ]; then
    echo "${MAINHS} does not exist."
    exit 1
fi

# parse options
OPTL=0
OPTJ=0

while getopts lj option 2> /dev/null
do
    case ${option} in
        l) OPTL=1 ;;
        j) OPTJ=1 ;;
        \?)
            echo "Specify one of the options -l or -j" 1>&2
            exit 1
            ;;
    esac
done

# required / exclusive check of options
[ $((OPTL + OPTJ)) -eq 0 ] && {
    echo "Specify one of the options -l or -j" 1>&2
    exit 1
}

[ $((OPTL + OPTJ)) -gt 1 ] && {
    echo "The options are mutually exclusive" 1>&2
    exit 1
}

if [ ${OPTL} -eq 1 ]; then
    # build for local environment
    cabal run bundler "Local" "${MAINHS}" "${BUNDLEDHS}"
    # ghc -package-env .package.local -o submission/a.out -O2 -prof -fprof-auto "${BUNDLEDHS}"
    ghc -package-env .package.local -o submission/a.out -O2 "${BUNDLEDHS}"

elif [ ${OPTJ} -eq 1 ]; then
    # build for judge environment
    cabal run bundler "Judge" "${MAINHS}" "${BUNDLEDHS}"
fi
