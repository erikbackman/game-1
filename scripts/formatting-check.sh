#!/usr/bin/env bash
set -euo pipefail

find "./src/app" "./src/lib" -iname '*.hs' -exec ormolu {} --mode=check \;
