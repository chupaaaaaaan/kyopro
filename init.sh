#!/bin/bash

set -euo pipefail
cd $(dirname $0)

. ghcenv.sh

cabal update && cabal build lib:kyopro --enable-profiling
