#!/usr/bin/env bash
# WASM build for xlfortran (JupyterLite kernel).
# Prerequisites:
#   1. emsdk 4.0.9 activated and sourced (emsdk_env.sh)
#   2. emscripten-forge build flags set (see debug.md)
#   3. xlfortran-wasm-host conda env created (environment-wasm-host.yml)
#   4. ./build0.sh already run (generated parser/ASR headers in source tree)

set -e
set -x

export PREFIX=${PREFIX:-$MAMBA_ROOT_PREFIX/envs/xlfortran-wasm-host}
export SYSROOT_PATH=${SYSROOT_PATH:-$EMSDK/upstream/emscripten/cache/sysroot}
export LLVM_WASM_BUILD=${LLVM_WASM_BUILD:-$MAMBA_ROOT_PREFIX/envs/llvm-test}

mkdir -p build-wasm

# Prevent conda-activated envs (e.g. lf) from leaking their native packages into
# the cross-compile search. cmake reads both the env var and the -D flag.
unset CMAKE_PREFIX_PATH

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
    -DSYSROOT_PATH="$SYSROOT_PATH" \
    "-DCMAKE_PREFIX_PATH=$PREFIX;$LLVM_WASM_BUILD" \
    -DLLVM_DIR="$LLVM_WASM_BUILD/lib/cmake/llvm" \
    -DLLD_DIR="$LLVM_WASM_BUILD/lib/cmake/lld" \
    -DBUILD_TESTING=OFF

emmake make -C build-wasm install -j8
