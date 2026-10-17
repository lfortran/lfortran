#!/usr/bin/env bash
# The experiments behind ANALYSIS.md: what LFortran's generated code would
# gain from each missing optimization, measured by applying the
# optimization by hand.
#
# CPU: LFortran's LLVM IR of nbody.f90 is rebuilt with LLVM 11's opt/llc
# (the LLVM LFortran uses) with
#   - `nsw` on the integer increments of the loops, and
#   - the ASR `fma` pass skipped (--skip-pass=fma),
# and, for OpenMP, with the force loop written as `!$omp parallel do
# schedule(dynamic)` instead of `do concurrent` (static schedule).
#
# GPU: ablation.m runs the force kernel LFortran generates and variants of it
# (counted loop, unrolling, threadgroup size, rsqrt, threadgroup memory).
#
# From the repository root:  pixi run -e llvm11 bash examples/nbody/ablation.sh
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
build=$here/build/analysis
mkdir -p "$build"
cd "$build"

LFORTRAN=${LFORTRAN:-$root/build/llvm11/src/bin/lfortran}
LLVM_BIN=${LLVM_BIN:-${CONDA_PREFIX:+$CONDA_PREFIX/bin}}
OPENMP_LIB_DIR=${OPENMP_LIB_DIR:-${CONDA_PREFIX:+$CONDA_PREFIX/lib}}
RUNTIME_DIR=$(cd "$(dirname "$LFORTRAN")/../runtime" && pwd)
CC=${CC:-/usr/bin/clang}
export PATH=/usr/bin:$PATH
N_SINGLE=${N_SINGLE:-8192}; N_MULTI=${N_MULTI:-8192}; N_GPU=${N_GPU:-8192}
STEPS=${STEPS:-5}; STEPS_GPU=${STEPS_GPU:-5}
gpu_run=(bash "${GPU_RUN:-$here/gpu_run.sh}")

gflops() {
    local output
    output=$("$@") || return $?
    printf '%s\n' "$output" | awk '/^RESULT/ {
        found++
        if (!($4 > 0 && $5 > 0 && $6 >= 0 && $6 < 1e-6)) exit 1
        print $5
    } END {if (found != 1) exit 1}'
}
best_of_3() {
    : > "$build/samples.log"
    for _ in 1 2 3; do
        gflops "$@" >> "$build/samples.log" || return $?
    done
    sort -g "$build/samples.log" | tail -1
}
report() {
    local name=$1 value
    shift
    value=$(best_of_3 "$@") || exit $?
    echo "$name: $value"
}

# LLVM IR -> executable, with `nsw` on `add i32 %x, 1` (the loop increments
# LFortran emits) when $2 is "nsw". LFortran prints `void*` in the OpenMP
# thread data type, which LLVM 11's parser rejects; it is `i8*`.
link_ir() {
    local ir=$1 nsw=$2 exe=$3
    if [ "$nsw" = nsw ]; then
        sed -E 's/= add i32 (%[0-9]+), 1$/= add nsw i32 \1, 1/; s/void\*/i8*/g' "$ir" > "$exe.ll"
    else
        sed -E 's/void\*/i8*/g' "$ir" > "$exe.ll"
    fi
    "$LLVM_BIN/opt" -O3 "$exe.ll" -o "$exe.bc"
    "$LLVM_BIN/llc" -O3 "$exe.bc" -o "$exe.s"
    "$CC" "$exe.s" -L"$RUNTIME_DIR" -Wl,-rpath,"$RUNTIME_DIR" -llfortran_runtime \
        ${OPENMP_LIB_DIR:+-L"$OPENMP_LIB_DIR" -Wl,-rpath,"$OPENMP_LIB_DIR" -lgomp} \
        -o "$exe"
}

echo "== single core, N = $N_SINGLE (GFLOP/s, best of 3)"
"$LFORTRAN" --fast --show-llvm "$here/nbody.f90" > fma.ll
"$LFORTRAN" --fast --skip-pass=fma --show-llvm "$here/nbody.f90" > nofma.ll
link_ir fma.ll plain cpu_as_is
link_ir fma.ll nsw cpu_nsw
link_ir nofma.ll plain cpu_nofma
link_ir nofma.ll nsw cpu_nsw_nofma
for v in cpu_as_is cpu_nsw cpu_nofma cpu_nsw_nofma; do
    report "$v" "./$v" "$N_SINGLE" "$STEPS"
done
for v in cpu_as_is cpu_nsw_nofma; do
    echo "loop-vectorize remarks, $v:"
    "$LLVM_BIN/opt" -O3 -pass-remarks=loop-vectorize -pass-remarks-missed=loop-vectorize \
        -pass-remarks-analysis=loop-vectorize "$v.ll" -o /dev/null 2>&1 | sort | uniq -c
done

echo "== OpenMP, N = $N_MULTI (GFLOP/s, best of 3)"
python3 - "$here/nbody.f90" dynamic.f90 <<'EOF'
import sys
s = open(sys.argv[1]).read()
a = s.index("        do concurrent (i = 1:n) &\n")
b = s.index("\n", s.index("local(j,", a))
s = (s[:a] + "        !$omp parallel do schedule(dynamic, 16) private(j, xi, yi, zi, "
     "axi, ayi, azi, dx, dy, dz, r2, inv_r, s)\n        do i = 1, n" + s[b:])
end = "            az(i) = azi\n        end do\n"
e = s.index(end) + len(end)
s = s[:e] + "        !$omp end parallel do\n" + s[e:]
open(sys.argv[2], "w").write(s)
EOF
"$LFORTRAN" --fast --openmp --show-llvm "$here/nbody.f90" > omp.ll
"$LFORTRAN" --fast --openmp --skip-pass=fma --show-llvm "$here/nbody.f90" > omp_nofma.ll
"$LFORTRAN" --fast --openmp --skip-pass=fma --show-llvm dynamic.f90 > omp_dynamic.ll
link_ir omp.ll plain omp_as_is
link_ir omp_nofma.ll nsw omp_nsw_nofma
link_ir omp_dynamic.ll nsw omp_nsw_nofma_dynamic
for v in omp_as_is omp_nsw_nofma omp_nsw_nofma_dynamic; do
    report "$v" "./$v" "$N_MULTI" "$STEPS"
done

echo "== GPU, N = $N_GPU (GFLOP/s, best of 3)"
"$CC" -O2 -fobjc-arc -framework Metal -framework Foundation \
    -DNBODY_ABLATION_SOURCE="\"$here/metal/ablation.metal\"" \
    "$here/metal/ablation.m" -o ablation
"$LFORTRAN" --fast --gpu=metal "$here/nbody.f90" -o lfortran_metal
"$LFORTRAN" --fast --skip-pass=fma --gpu=metal "$here/nbody.f90" -o lfortran_metal_nofma
report "lfortran --fast --gpu=metal" "${gpu_run[@]}" ./lfortran_metal "$N_GPU" "$STEPS_GPU"
report "  the same, without the fma pass" "${gpu_run[@]}" ./lfortran_metal_nofma "$N_GPU" "$STEPS_GPU"
while read -r variant group unroll inv_sqrt; do
    report "ablation $variant threadgroup=$group unroll=$unroll $inv_sqrt" \
        "${gpu_run[@]}" ./ablation "$N_GPU" "$STEPS_GPU" "$variant" "$group" "$unroll" "$inv_sqrt"
done <<'EOF'
lfortran 256 1 div
lfortran 128 1 div
unrolled 256 1 div
unrolled 256 4 div
unrolled 256 4 rsqrt
unrolled 128 4 div
unrolled 128 8 div
tiled 128 4 div
EOF
echo "per-step overhead, N = 1024, 100 steps (time in s):"
output=$("${gpu_run[@]}" ./lfortran_metal 1024 100)
echo "  lfortran --gpu=metal: $(printf '%s\n' "$output" | awk '/^RESULT/ {print $4}')"
output=$("${gpu_run[@]}" ./ablation 1024 100 lfortran 256 1 div)
echo "  ablation (LFortran's kernels, 1 command buffer per step):" \
    "$(printf '%s\n' "$output" | awk '/^RESULT/ {print $4}')"
