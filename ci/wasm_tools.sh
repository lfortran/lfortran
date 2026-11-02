#!/usr/bin/env bash
# Download the toolchains of the LLVM WASM backends (llvm_wasm and
# llvm_wasm_emcc) into LFORTRAN_WASM_TOOLS:
#
#     pixi run -e llvm19-wasm wasm-tools
#
# conda-forge packages neither the WASI SDK, the wasmtime command line tool
# nor emsdk 3.1.35, so this fetches pinned releases and checks their SHA-256.
# Tools that are already installed are kept.

set -euo pipefail

dest="${LFORTRAN_WASM_TOOLS:?run this with pixi run -e llvm19-wasm wasm-tools}"
mkdir -p "$dest"
cd "$dest"

fetch() {
    local url="$1" sha256="$2" out="$3"
    curl -fsSL --retry 3 -o "$out" "$url"
    echo "$sha256  $out" | sha256sum --check -
}

if [[ ! -x wasi-sdk/bin/clang ]]; then
    fetch https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-21/wasi-sdk-21.0-linux.tar.gz \
        f2fe0723b337c484556b19d64c0f6c6044827014bfcd403d00951c65a86cfa26 wasi-sdk.tar.gz
    rm -rf wasi-sdk wasi-sdk-21.0
    tar -xzf wasi-sdk.tar.gz
    mv wasi-sdk-21.0 wasi-sdk
    rm wasi-sdk.tar.gz
fi
wasi-sdk/bin/clang --version

if [[ ! -x wasmtime/wasmtime ]]; then
    fetch https://github.com/bytecodealliance/wasmtime/releases/download/v19.0.2/wasmtime-v19.0.2-x86_64-linux.tar.xz \
        b4dbfb73b5434bfea92b495fd8bb4721b6cd58d193f892ddbb1f89cfa85867f5 wasmtime.tar.xz
    rm -rf wasmtime wasmtime-v19.0.2-x86_64-linux
    tar -xJf wasmtime.tar.xz
    mv wasmtime-v19.0.2-x86_64-linux wasmtime
    rm wasmtime.tar.xz
fi
wasmtime/wasmtime --version

# emsdk then downloads the Emscripten 3.1.35 release itself.
if [[ ! -x emsdk/upstream/emscripten/emcc ]]; then
    fetch https://github.com/emscripten-core/emsdk/archive/refs/tags/4.0.13.tar.gz \
        83b6443aa3c1af248a960e2ab29cf90954a84400a93a5ce595a6c3c602e865bf emsdk.tar.gz
    rm -rf emsdk emsdk-4.0.13
    tar -xzf emsdk.tar.gz
    mv emsdk-4.0.13 emsdk
    rm emsdk.tar.gz
    (cd emsdk && ./emsdk install 3.1.35 && ./emsdk activate 3.1.35)
fi
emsdk/upstream/emscripten/emcc --version
