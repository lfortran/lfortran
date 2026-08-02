#!/usr/bin/env bash

set -ex

if [[ -z "${LFORTRAN_VERSION:-}" ]]; then
    echo "LFORTRAN_VERSION must be set by CI" >&2
    exit 1
fi

lfortran_version=$LFORTRAN_VERSION

cd ./dist


RELEASE_ID=$(\
curl -s 'https://api.github.com/repos/lfortran/lfortran/releases/latest' | \
python -c "import sys, json; print(json.load(sys.stdin)['id'], end='')")

echo "RELEASE_ID=$RELEASE_ID"

curl --fail -L \
-X POST \
-H "Accept: application/vnd.github+json" \
-H "Authorization: Bearer $GITHUB_TOKEN" \
-H "X-GitHub-Api-Version: 2022-11-28" \
-H "Content-Type: application/octet-stream" \
"https://uploads.github.com/repos/lfortran/lfortran/releases/$RELEASE_ID/assets?name=lfortran-${lfortran_version}.tar.gz" \
--data-binary "@lfortran-${lfortran_version}.tar.gz"
