#!/usr/bin/env bash
# Fast cold setup of the Lake dependencies, used by CI:
# 1. Shallow-fetch each package of lake-manifest.json at its pinned revision.
#    Lake accepts these checkouts as is; its own full clone of Mathlib's
#    history would take much longer.
# 2. Download prebuilt Mathlib .olean files, but only for the Mathlib modules
#    imported here and their dependencies (~1000 of Mathlib's ~8000 modules).
set -ex
cd "$(dirname "$0")"

python3 - <<'PY' | while read -r name url rev; do
import json
for p in json.load(open("lake-manifest.json"))["packages"]:
    print(p["name"], p["url"], p["rev"])
PY
    dir=".lake/packages/$name"
    if [ "$(git -C "$dir" rev-parse HEAD 2>/dev/null)" = "$rev" ]; then
        continue
    fi
    rm -rf "$dir"
    git init -q "$dir"
    git -C "$dir" remote add origin "$url"
    git -C "$dir" fetch -q --depth 1 origin "$rev"
    git -C "$dir" checkout -q FETCH_HEAD
done

lake exe cache get $(grep -rh '^import Mathlib' LFortran Legendre \
    | sed 's/^import //' | sort -u)
