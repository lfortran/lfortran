#!/usr/bin/env bash

set -e
set -x

cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=yes \
    -DLFORTRAN_BUILD_ALL=yes \
    -DWITH_STACKTRACE=yes \
    -DWITH_LIBUNWIND=yes \
    -DWITH_RUNTIME_STACKTRACE=yes \
    -DWITH_LSP=yes \
    -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX=`pwd`/inst \
    -DCMAKE_INSTALL_LIBDIR=share/lfortran/lib \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=yes \
    .
cmake --build . -j16 --target install
