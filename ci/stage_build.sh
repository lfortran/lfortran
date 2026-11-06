#!/usr/bin/env shell
# This is a cross-platform `shell` script.

# `ci/stage_build.sh <build_dir>`: copy the compiler and runtime built in
# <build_dir> into src/, for the test scripts that run the in-tree compiler.
# Pass the build directory relative to the project root: forward slashes keep
# the globs below working on Windows.

set -ex

build_dir="$1"

if [[ $WIN == "1" ]]; then # Windows
    cp "$build_dir/src/bin/lfortran.exe" src/bin
    cp "$build_dir"/src/runtime/lfortran_runtime* src/runtime/
else # Linux or macOS
    cp "$build_dir/src/bin/lfortran" src/bin
    cp "$build_dir"/src/runtime/liblfortran_runtime* src/runtime/
fi
cp "$build_dir"/src/runtime/*.mod src/runtime/
