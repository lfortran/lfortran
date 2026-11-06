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
off_stderr="$build_dir/test_gpu_metal_runtime_off.err"
on_stderr="$build_dir/test_gpu_metal_runtime_on.err"
exit_stderr="$build_dir/test_gpu_metal_runtime_exit.err"
rm -f "$off_stderr" "$on_stderr" "$exit_stderr"
# What the runs print to stderr is shown when the script fails.
show_stderr() {
    local status=$?
    if [[ $status != 0 ]]; then
        cat "$off_stderr" "$on_stderr" "$exit_stderr" >&2 2>/dev/null || true
    fi
    return $status
}
trap show_stderr EXIT
off_output=$("${gpu_run[@]}" env -u LFORTRAN_GPU_PROFILE "$binary" \
    2>"$off_stderr")
on_output=$("${gpu_run[@]}" env LFORTRAN_GPU_PROFILE=1 "$binary" \
    2>"$on_stderr")
# Without lfortran_gpu_shutdown, as a Fortran program ends.
"${gpu_run[@]}" env LFORTRAN_GPU_PROFILE=1 "$binary" --no-shutdown \
    >/dev/null 2>"$exit_stderr"

# The telemetry is printed to stderr, once, only when profiling is on: at
# shutdown, or when the program ends.
telemetry='^lfortran_gpu: commits=[1-9][0-9]* completed=[1-9][0-9]* dispatches=[1-9]'
if grep -q '^lfortran_gpu:' "$off_stderr"; then
    echo "telemetry printed with profiling off" >&2
    exit 1
fi
for err in "$on_stderr" "$exit_stderr"; do
    if [[ $(grep -c "$telemetry" "$err") != 1 ]]; then
        echo "telemetry not printed once with LFORTRAN_GPU_PROFILE=1" \
            "($err)" >&2
        exit 1
    fi
done

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
