#!/usr/bin/env bash
# WASM kernel build (Phase 2).  Run in the 'xeus-lfortran-wasm-build' env.
#
# Prerequisites:
#   - ./build0.sh already run
#   - ./wasm-build0.sh already run  (installs .mod files to $PREFIX/lib/)
#   - xeus-lfortran-wasm-host and xeus-lfortran-wasm-build envs created

set -ex

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

if [ "$(uname)" = "Darwin" ]; then
    CORES=$(sysctl -n hw.ncpu)
else
    CORES=$(nproc)
fi
emmake make -C build-wasm install -j$CORES
