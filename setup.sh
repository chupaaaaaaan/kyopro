#!/bin/bash

for problem_path in "$@"; do

    pathConfirm1=$(echo ${problem_path} | tr '/' ' ' | cut -d' ' -f1 | xargs realpath)
    pathConfirm2=$(pwd)/$(echo ${problem_path} | tr '/' ' ' | cut -d' ' -f1)

    if [ "${pathConfirm1}" != "${pathConfirm2}" ]; then
        echo "invalid problem path."
        exit 1
    fi

    if [ ! -s "${problem_path}/Main.hs" ]; then
        mkdir -p ${problem_path}
        cat template.hs > ${problem_path}/Main.hs
        echo "create file ${problem_path}/Main.hs from template.hs."
    fi
done
