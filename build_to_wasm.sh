#!/usr/bin/env bash

set -ex

./build1.sh

mkdir -p src/bin/asset_dir
cp src/runtime/*.mod src/bin/asset_dir
git clean -dfx -e src/bin/asset_dir

emcmake cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_CXX_FLAGS_DEBUG="-Wall -Wextra -fexceptions" \
    -DWITH_LLVM=no \
    -DLFORTRAN_BUILD_ALL=yes \
    -DLFORTRAN_BUILD_TO_WASM=yes \
    -DWITH_STACKTRACE=no \
    -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX=`pwd`/inst \
    .
cmake --build . -j16
