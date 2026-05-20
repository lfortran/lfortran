#!/usr/bin/env bash
# WASM build for xlfortran (JupyterLite kernel).
# Prerequisites:
#   1. emsdk 4.0.9 activated through xeus-lfortran-wasm-host environment (see environment-wasm-host.yml)
#   2. xeus-lfortran-wasm-host conda env created (environment-wasm-host.yml)
#   3. ./build0.sh already run (generated parser/ASR headers in source tree)

set -e
set -x

export PREFIX=${PREFIX:-$MAMBA_ROOT_PREFIX/envs/xeus-lfortran-wasm-host}

mkdir -p build-wasm

# Use explicit -S/-B so cmake never confuses the source-tree's in-source
# CMakeCache.txt (from build1.sh) with the wasm binary directory.
emcmake cmake -S . -B build-wasm \
    -DCMAKE_BUILD_TYPE=Release \
    -DLFORTRAN_BUILD_ALL=no \
    -DWITH_LLVM=yes \
    -DXEUS_LFORTRAN_WASM_BUILD=yes \
    -DWITH_XEUS=no \
    -DWITH_ZSTD=no \
    -DWITH_RUNTIME_LIBRARY=no \
    -DWITH_STACKTRACE=no \
    -DWITH_WHEREAMI=no \
    -DWITH_ZLIB=no \
    -DCMAKE_INSTALL_PREFIX="$PREFIX" \
    -DCMAKE_FIND_ROOT_PATH="$PREFIX" \
    -DCMAKE_PREFIX_PATH="$PREFIX" \
    -DLLVM_DIR="$PREFIX/lib/cmake/llvm" \
    -DLLD_DIR="$PREFIX/lib/cmake/lld"

emmake make -C build-wasm install -j8
