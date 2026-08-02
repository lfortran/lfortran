#!/usr/bin/env bash

set -ex

if [[ -z "$1" ]]; then
    echo "source tarball version must be provided"
    exit 1
fi

lfortran_version=$1
export dest=lfortran-$lfortran_version
bash -x -o errexit ci/create_source_tarball0.sh "$dest" "$lfortran_version"
