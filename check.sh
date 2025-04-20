#!/bin/bash

set -euo pipefail
cd $(dirname $0)

# build
./build.sh -l

# test
oj t -d cases/sample -c submission/a.out
