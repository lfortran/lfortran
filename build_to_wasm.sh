#!/usr/bin/env bash

set -ex

cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DWITH_LLVM=yes \
    -DLFORTRAN_BUILD_ALL=yes \
    -DWITH_STACKTRACE=no \
    -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX=`pwd`/inst \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=yes \
    .
cmake --build . -j16 --target install

mkdir -p src/bin/asset_dir
cp src/runtime/*.mod src/bin/asset_dir
git clean -dfx -e src/bin/asset_dir

emcmake cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_FLAGS_DEBUG="-Wall -Wextra -fexceptions" \
    -DCMAKE_CXX_FLAGS_RELEASE="-Wall -Wextra -fexceptions" \
    -DWITH_LLVM=no \
    -DLFORTRAN_BUILD_ALL=yes \
    -DLFORTRAN_BUILD_TO_WASM=yes \
    -DWITH_STACKTRACE=no \
    -DWITH_LSP=no \
    -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX=`pwd`/inst \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=yes \
    .
cmake --build . -j16
