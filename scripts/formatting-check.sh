#!/usr/bin/env bash
set -euo pipefail

files=$(find "./src/app" "./src/lib" -iname '*.hs')
for file in $files
do
    ormolu $file --mode=check
done
