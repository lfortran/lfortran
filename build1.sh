#!/usr/bin/env bash

set -e
set -x

cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DWITH_LLVM=yes \
    -DLFORTRAN_BUILD_ALL=no \
    -DWITH_STACKTRACE=no \
    -DWITH_RUNTIME_STACKTRACE=no \
    -DWITH_LSP=no \
    -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX=`pwd`/inst \
    -DCMAKE_INSTALL_LIBDIR=share/lfortran/lib \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=yes \
-G Ninja \
    .
cmake --build . --target install
