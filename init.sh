#!/bin/bash

set -euo pipefail
cd $(dirname $0)

. ghcenv.sh

cabal update && cabal build lib:kyopro --write-ghc-environment-files=always --enable-profiling

mv .ghc.environment.x86_64-linux-9.4.5 .package.local
