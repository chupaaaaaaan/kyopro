#!/bin/bash

problem_path=$1

if [ -z "${problem_path}" ]; then
    echo "problem path is required."
    exit 1
fi

pathConfirm1=$(echo ${problem_path} | tr '/' ' ' | cut -d' ' -f1 | xargs realpath)
pathConfirm2=$(pwd)/$(echo ${problem_path} | tr '/' ' ' | cut -d' ' -f1)

if [ "${pathConfirm1}" != "${pathConfirm2}" ]; then
    echo "invalid problem path."
    exit 2
fi

problem_name=`echo ${problem_path} | tr '/' '-'`

if [ ! $(cat package.yaml | grep -q "^  ${problem_name}") ]; then
    cat <<EOF >> package.yaml
  ${problem_name}:
    main: Main.hs
    source-dirs: ${problem_path}
    dependencies:
    - kyopro

EOF
    mkdir -p ${problem_path}
    touch ${problem_path}/Main.hs
fi
