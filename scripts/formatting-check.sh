#!/usr/bin/env bash
set -euo pipefail
pushd $PWD/.. &> /dev/null
find "./src/app" "./src/lib" -iname '*.hs' -exec ormolu {} --mode=check \;
popd &> /dev/null
