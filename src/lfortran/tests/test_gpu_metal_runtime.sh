#!/usr/bin/env bash
set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "$here/../../.." && pwd)
build_dir=${LFORTRAN_BUILD_DIR:-"$repo/build/llvm11"}
binary="$build_dir/test_gpu_metal_runtime"

mkdir -p "$build_dir"

xcrun --sdk macosx clang -O2 -fobjc-arc \
    -I"$repo/src/libasr/runtime" \
    -framework Foundation -framework Metal \
    "$here/test_gpu_metal_runtime.m" \
    "$repo/src/libasr/runtime/lfortran_gpu_metal.m" \
    -o "$binary"

gpu_run=(bash "$repo/examples/nbody/gpu_run.sh")
off_output=$("${gpu_run[@]}" env -u LFORTRAN_GPU_PROFILE "$binary")
on_output=$("${gpu_run[@]}" env LFORTRAN_GPU_PROFILE=1 "$binary")

printf '%s\n' "$off_output"
printf '%s\n' "$on_output"

off_sum=$(printf '%s\n' "$off_output" | awk '
    /^RESULT / {
        for (i = 1; i <= NF; i++) {
            if ($i ~ /^final_sum=/) {
                split($i, pair, "=");
                print pair[2];
            }
        }
    }')
on_sum=$(printf '%s\n' "$on_output" | awk '
    /^RESULT / {
        for (i = 1; i <= NF; i++) {
            if ($i ~ /^final_sum=/) {
                split($i, pair, "=");
                print pair[2];
            }
        }
    }')

if [[ "$off_sum" != "$on_sum" ]]; then
    echo "final sums differ between profiling off and on: $off_sum vs $on_sum" >&2
    exit 1
fi
