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
profile_file="$build_dir/test_gpu_metal_runtime_profile.txt"
rm -f "$profile_file"
off_output=$("${gpu_run[@]}" env -u LFORTRAN_GPU_PROFILE "$binary")
on_output=$("${gpu_run[@]}" env LFORTRAN_GPU_PROFILE=1 "$binary")
kernels_output=$("${gpu_run[@]}" env LFORTRAN_GPU_PROFILE=2 \
    LFORTRAN_GPU_PROFILE_FILE="$profile_file" "$binary")

printf '%s\n' "$off_output"
printf '%s\n' "$on_output"
printf '%s\n' "$kernels_output"
cat "$profile_file"

final_sum() {
    printf '%s\n' "$1" | awk '
        /^RESULT / {
            for (i = 1; i <= NF; i++) {
                if ($i ~ /^final_sum=/) {
                    split($i, pair, "=");
                    print pair[2];
                }
            }
        }'
}
off_sum=$(final_sum "$off_output")
on_sum=$(final_sum "$on_output")
kernels_sum=$(final_sum "$kernels_output")

if [[ "$off_sum" != "$on_sum" || "$off_sum" != "$kernels_sum" ]]; then
    echo "final sums differ between profiling off, on and per kernel:" \
        "$off_sum, $on_sum, $kernels_sum" >&2
    exit 1
fi

# The one kernel was launched 317 times by the phases and 6 times by the
# chunks check, each launch timed on the GPU.
if ! awk '
    $NF == "telemetry_step" && $2 == 323 && $3 > 0 && $5 == 100.00 { found = 1 }
    END { exit !found }' "$profile_file"; then
    echo "the per-kernel profile does not show 323 timed launches" \
        "of telemetry_step" >&2
    exit 1
fi
