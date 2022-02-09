#!/bin/bash

cd $(dirname $0)

for path in "$@"
do
    if [ ! -f "${path}/Main.hs" ]
    then
        mkdir -p ${path}
        cat template.hs > ${path}/Main.hs
        echo "create file ${path}/Main.hs from template.hs."
    fi
done
