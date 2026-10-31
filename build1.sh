#!/usr/bin/env bash

# `pixi run build [cmake_args...]` runs this with the Prefix `shell` as
# `build1.sh <build_dir> [cmake_args...]`. Keep to syntax that Bash also
# supports.

set -e
set -x

build_dir="${1:-.}"
if [ "$#" -gt 0 ]; then
    shift
fi
# Build another source tree, such as an unpacked source tarball.
source_dir="${LFORTRAN_SOURCE_DIR:-.}"

cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=yes \
    -DLFORTRAN_BUILD_ALL=yes \
    -DWITH_STACKTRACE=yes \
    -DWITH_RUNTIME_STACKTRACE=yes \
    -DWITH_LSP=no \
    -DWITH_INTERNAL_ALLOC_CHECK=yes \
    -DUSE_DYNAMIC_ZSTD=no \
    -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX" \
    -DCMAKE_INSTALL_LIBDIR=share/lfortran/lib \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_COLOR_DIAGNOSTICS=ON \
    -G Ninja \
    -S "$source_dir" -B "$build_dir" \
    "$@"
cmake --build "$build_dir"
