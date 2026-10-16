#!/usr/bin/env bash

# `pixi run build [cmake_args...]` runs this with the Prefix `shell` through
# ci/build1.py, which forwards the CMake arguments (`shell` cannot pass
# arguments to scripts). Keep to syntax that Bash also supports.

set -e
set -x

build_dir="${LFORTRAN_BUILD_DIR:-.}"

python ci/build1.py --cmake \
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
    -S . -B "$build_dir"
cmake --build "$build_dir"
