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
    -DCMAKE_INSTALL_LIBDIR=share/lfortran/lib \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_FLAGS="${CFLAGS} -fdiagnostics-color=always" \
    -DCMAKE_CXX_FLAGS="${CXXFLAGS} -fdiagnostics-color=always" \
-G Ninja \
    .
cmake --build .
