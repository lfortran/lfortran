# Pixi activation script of the llvm-wasm feature (see pixi.toml): put the
# wasmtime downloaded by ci/wasm_tools.sh on PATH. An activation.env PATH
# would replace the build feature's PATH instead of extending it.
export PATH="$PIXI_PROJECT_ROOT/.wasm-tools/wasmtime:$PATH"
