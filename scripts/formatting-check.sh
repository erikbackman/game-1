#!/usr/bin/env bash
set -euo pipefail

files=$(find "./src" -iname '*.hs' -not -path "./src/dist-newstyle/*")
for file in $files
do
    ormolu $file --mode=check
done
