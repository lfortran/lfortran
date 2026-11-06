#!/usr/bin/env bash
set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "$here/../../.." && pwd)
build_dir=${LFORTRAN_BUILD_DIR:-"$repo/build/llvm11"}
binary="$build_dir/test_gpu_metal_memory"

mkdir -p "$build_dir"

xcrun --sdk macosx clang -O2 -fobjc-arc \
    -I"$repo/src/libasr/runtime" \
    -framework Foundation -framework Metal \
    "$here/test_gpu_metal_memory.m" \
    "$repo/src/libasr/runtime/lfortran_gpu_metal.m" \
    -o "$binary"

gpu_run=(bash "$repo/examples/nbody/gpu_run.sh")
for model in managed copy; do
    "${gpu_run[@]}" env LFORTRAN_GPU_PROFILE=1 LFORTRAN_GPU_MEMORY=$model \
        "$binary"
done
