#!/usr/bin/env bash
# SC2016: '$...' in single quotes expands inside the Pixi environment.
# SC2054: --cells n,m is one argument. SC2329: functions are run by step.
# shellcheck disable=SC2016,SC2054,SC2329
# Validate and benchmark LFortran's native CUDA backend on an NVIDIA GPU with
# Formal (https://github.com/BerkeleyLab/formal) and LFortran's own CUDA tests.
# See ci/cuda_nvidia/README.md. Every step writes its own log under
# WORK_DIR/logs; a summary is printed at the end and the exit code is nonzero
# if a step failed. Nothing outside WORK_DIR is written except the Pixi
# environments and LFortran's integration test build directory.
#
# Usage: ci/cuda_nvidia/run_formal_cuda.sh [WORK_DIR]
# Environment overrides:
#   FORMAL_REPO   (default https://github.com/BerkeleyLab/formal.git)
#   FORMAL_REF    (default: the validated Formal revision below)
#   GPU_ARCH      (default native; e.g. sm_120 when compiling without a GPU)
#   JOBS          (default 16)
#   BENCH_CELLS   (default 2048,2048)  BENCH_STEPS (default 1000)
#   BENCH_RUNS    (default 5)
#   SKIP_LFORTRAN_TESTS=1  skip `integration_tests -b cuda` and
#                          src/tests/gpu_cuda_scratch_order.sh
#   SKIP_PROFILE=1         skip nsys / compute-sanitizer
#   SKIP_BENCHMARK=1       skip the timing runs
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LFORTRAN_ROOT="$(cd "$HERE/../.." && pwd)"
WORK_ARG="${1:-$PWD/formal-cuda-work}"
mkdir -p "$WORK_ARG"
WORK="$(cd "$WORK_ARG" && pwd)"
FORMAL_REPO="${FORMAL_REPO:-https://github.com/BerkeleyLab/formal.git}"
# Head of BerkeleyLab/formal#71 (which contains #70): the RK4 workspace
# solver, raw input/output and the comparison scripts used below.
FORMAL_REF="${FORMAL_REF:-ef4fb6e1cfb6eed69c588e1e92fbf739ebcb09bf}"
GPU_ARCH="${GPU_ARCH:-native}"
JOBS="${JOBS:-16}"
BENCH_CELLS="${BENCH_CELLS:-2048,2048}"
BENCH_STEPS="${BENCH_STEPS:-1000}"
BENCH_RUNS="${BENCH_RUNS:-5}"
FORMAL_ROOT="$WORK/formal"
V="$WORK/validation"
LOGS="$WORK/logs"
mkdir -p "$V" "$LOGS"
export LFORTRAN_ROOT FORMAL_ROOT

LFORTRAN_COMPILER="$LFORTRAN_ROOT/build/llvm11/src/bin/lfortran"
FPM_MANIFEST="$HERE/fpm/pixi.toml"
GF16_MANIFEST="$HERE/gfortran16/pixi.toml"
# nsys and compute-sanitizer write to $TMPDIR; use a private one, since a
# shared /tmp/nvidia may not be writable.
export TMPDIR="$WORK/tmp"
mkdir -p "$TMPDIR"

declare -a SUMMARY=()
FAILED=0
step() { # step NAME LOG CMD...
    local name="$1" log="$2" rc=0
    shift 2
    echo "==> $name (log: $log)"
    "$@" > "$log" 2>&1 || rc=$?
    if [ "$rc" -eq 0 ]; then
        SUMMARY+=("PASS  $name")
    else
        SUMMARY+=("FAIL  $name (exit $rc, $log)")
        FAILED=1
    fi
}
lf() { pixi run --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 "$@"; }
formal_fpm() {
    lf pixi run --manifest-path "$FPM_MANIFEST" \
        /bin/bash -c 'cd "$FORMAL_ROOT" && exec fpm "$@"' _ "$@"
}
gfortran_fpm() {
    pixi run --manifest-path "$GF16_MANIFEST" \
        /bin/bash -c 'cd "$FORMAL_ROOT" && exec fpm "$@"' _ "$@"
}

# 0. Toolchain and revisions.
{
    echo "date: $(date -Is)"
    echo "lfortran_head: $(git -C "$LFORTRAN_ROOT" rev-parse HEAD)"
    nvidia-smi --query-gpu=name,compute_cap,driver_version --format=csv,noheader
    nvidia-smi -q | grep -i 'Addressing Mode' || true
    lf bash -c 'command -v lfortran; lfortran --version | head -1; llvm-config --version;
        command -v nvcc; nvcc --version | tail -2; command -v c++; c++ --version | head -1'
} > "$LOGS/00-environment.log" 2>&1 || true
cat "$LOGS/00-environment.log"
test -x "$LFORTRAN_COMPILER" || { echo "Build LFortran first: pixi run -e llvm11 build (see README.md)"; exit 1; }
NVCC="$(lf bash -c 'command -v nvcc' 2>/dev/null || true)"
CUDA_CPU_CXX="$(lf bash -c 'command -v "${CXX:-c++}"' 2>/dev/null)"
test -n "$NVCC" && test -x "$NVCC" || { echo "nvcc not found on PATH"; exit 1; }
CUDA_BIN="$(dirname "$NVCC")"

# 1. Formal at the validated revision.
if [ ! -d "$FORMAL_ROOT/.git" ]; then
    git clone -q "$FORMAL_REPO" "$FORMAL_ROOT"
fi
git -C "$FORMAL_ROOT" fetch -q origin "$FORMAL_REF" 2>/dev/null \
    || git -C "$FORMAL_ROOT" fetch -q origin pull/71/head
git -C "$FORMAL_ROOT" checkout -q --detach "$FORMAL_REF"
echo "formal_head: $(git -C "$FORMAL_ROOT" rev-parse HEAD)" | tee -a "$LOGS/00-environment.log"

# Keep all flags in one --flag argument.
COMMON_FLAGS="--cpp --realloc-lhs-arrays --separate-compilation"
CPU_FLAGS="$COMMON_FLAGS --gpu=cuda_cpu --device-compiler=$CUDA_CPU_CXX"
NATIVE_FLAGS="$COMMON_FLAGS --gpu=cuda --gpu-arch=$GPU_ARCH --device-compiler=$NVCC"
CPU_BUILD="$FORMAL_ROOT/build/cuda-cpu"
NATIVE_BUILD="$FORMAL_ROOT/build/cuda-nvidia"
HOST_BUILD="$FORMAL_ROOT/build/llvm-cpu-reference"
GF_BUILD="$FORMAL_ROOT/build/gfortran16-reference"
EXAMPLE=(--example 2D-advection-diffusion)
GAUSS_ARGS=(--solver workspace --order 4 --cells 17,13 --steps 9 --dt 1e-4 --no-csv)
OPS_ARGS=(--solver operators --order 4 --cells 9,11 --steps 2 --dt 1e-4 --no-csv)
WS_INPUT_ARGS=(--solver workspace --order 4 --no-csv --input "$V/gaussian-input.fadv2d")

# 2. CUDA CPU emulation.
step "formal tests, cuda_cpu" "$LOGS/10-fpm-test-cuda-cpu.log" \
    formal_fpm test --compiler "$LFORTRAN_COMPILER" --build-dir "$CPU_BUILD" --flag "$CPU_FLAGS"
step "operators solver, cuda_cpu" "$LOGS/11-operators-cuda-cpu.log" \
    formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$CPU_BUILD" --flag "$CPU_FLAGS" \
        "${EXAMPLE[@]}" -- "${OPS_ARGS[@]}"
step "workspace solver, cuda_cpu (dumps shared input)" "$LOGS/12-workspace-cuda-cpu.log" \
    formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$CPU_BUILD" --flag "$CPU_FLAGS" \
        "${EXAMPLE[@]}" -- "${GAUSS_ARGS[@]}" \
        --dump-input "$V/gaussian-input.fadv2d" --dump-final-raw "$V/gaussian-cuda-cpu-final.fadv2d"

# Every `do concurrent` is offloaded. Without --gpu-allow-cpu-fallback a loop
# LFortran cannot offload is a compile error; with it, such a loop is
# reported as "parallel loop runs on the CPU" instead. Building all of
# Formal (library, tests, every example) with the fallback allowed and
# finding no such report shows that none would run on the host.
all_do_concurrent_offloaded() {
    local b="$FORMAL_ROOT/build/fallback-check" log="$LOGS/19-fallback-build.log"
    local f="$NATIVE_FLAGS --gpu-allow-cpu-fallback" src ex n_loops
    formal_fpm build --compiler "$LFORTRAN_COMPILER" --build-dir "$b" --flag "$f" --tests > "$log" 2>&1 || return 1
    for src in "$FORMAL_ROOT"/example/*.[Ff]90; do
        grep -qiE '^\s*program\b' "$src" || continue
        ex="$(basename "$src")"
        ex="${ex%.*}"
        formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$b" --flag "$f" \
            --example "$ex" --runner true >> "$log" 2>&1 || return 1
    done
    n_loops="$(grep -rciE '^\s*([a-z_0-9]+\s*:\s*)?do\s+concurrent' \
        "$FORMAL_ROOT/src" "$FORMAL_ROOT/test" "$FORMAL_ROOT/example" | awk -F: '{s+=$2} END {print s}')"
    echo "do concurrent constructs in Formal: $n_loops"
    echo "loops reported as running on the CPU: $(grep -c 'runs on the CPU' "$log" || true)"
    ! grep -q 'runs on the CPU' "$log"
}
step "every do concurrent offloaded (no CPU fallback)" "$LOGS/19-all-do-concurrent-offloaded.log" \
    all_do_concurrent_offloaded

# 3. Native CUDA. With PTX JIT disabled a binary built for the wrong
# architecture fails instead of silently JIT compiling.
export CUDA_DISABLE_PTX_JIT=1
step "formal tests, native cuda" "$LOGS/20-fpm-test-cuda-native.log" \
    formal_fpm test --compiler "$LFORTRAN_COMPILER" --build-dir "$NATIVE_BUILD" --flag "$NATIVE_FLAGS"
step "operators solver, native cuda" "$LOGS/21-operators-cuda-native.log" \
    formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$NATIVE_BUILD" --flag "$NATIVE_FLAGS" \
        "${EXAMPLE[@]}" -- "${OPS_ARGS[@]}"
# --input supplies cells, steps and dt; Formal rejects them together.
step "workspace solver, native cuda (shared input)" "$LOGS/22-workspace-cuda-native.log" \
    formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$NATIVE_BUILD" --flag "$NATIVE_FLAGS" \
        "${EXAMPLE[@]}" -- "${WS_INPUT_ARGS[@]}" --dump-final-raw "$V/gaussian-cuda-native-final.fadv2d"

# 4. Executables: native GPU, host LLVM, independent GFortran 16 oracle.
step "capture native executable" "$LOGS/30-capture-native.log" \
    formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$NATIVE_BUILD" --flag "$NATIVE_FLAGS" \
        "${EXAMPLE[@]}" --runner cp -- "$V/formal-native"
step "capture host LLVM executable" "$LOGS/31-capture-host.log" \
    formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$HOST_BUILD" --flag "$COMMON_FLAGS" \
        "${EXAMPLE[@]}" --runner cp -- "$V/formal-host-llvm"
step "capture GFortran 16 executable" "$LOGS/32-capture-gfortran.log" \
    gfortran_fpm run --compiler gfortran --build-dir "$GF_BUILD" \
        --flag "-cpp -O2 -ffree-line-length-none -fcoarray=single" \
        "${EXAMPLE[@]}" --runner cp -- "$V/formal-gfortran"
GF_LIB="$(pixi run --manifest-path "$GF16_MANIFEST" bash -c 'echo "$CONDA_PREFIX/lib"' 2>/dev/null)"

# 5. Full-field comparisons.
step "gaussian 17x13/9: host LLVM reference" "$LOGS/40-gaussian-host.log" \
    "$V/formal-host-llvm" "${WS_INPUT_ARGS[@]}" --dump-final-raw "$V/gaussian-host-final.fadv2d"
step "gaussian 17x13/9: native vs host (validate_2d_advection_workspace.py)" "$LOGS/41-gaussian-validate.log" \
    lf python "$FORMAL_ROOT/example/scripts/validate_2d_advection_workspace.py" \
        --executable "$V/formal-native" --workdir "$V/compare-native" \
        --raw-input "$V/gaussian-input.fadv2d" --reference-raw "$V/gaussian-host-final.fadv2d" \
        --cells 17,13 --steps 9 --dt 1e-4 --evolution --gpu-runner env

step "boundary 13x11/3: input" "$LOGS/50-boundary-input.log" \
    lf python "$HERE/make_boundary_input.py" "$V/boundary-input.fadv2d" 13 11 3 1e-4
# The comparator takes the oracle payloads without the FADV2D1 header.
strip_header() {
    lf python -c 'import sys; b=open(sys.argv[1],"rb").read().split(b"\n",3); assert b[2]==b"mode raw-final"; open(sys.argv[2],"wb").write(b[3])' \
        "$1" "$2"
}
for exe in native host-llvm gfortran; do
    step "boundary 13x11/3: $exe" "$LOGS/51-boundary-$exe.log" \
        env LD_LIBRARY_PATH="$GF_LIB${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" "$V/formal-$exe" \
            --solver workspace --order 4 --no-csv --input "$V/boundary-input.fadv2d" \
            --dump-final-raw "$V/boundary-$exe-final.fadv2d"
    step "boundary 13x11/3: $exe payload" "$LOGS/51-boundary-$exe-payload.log" \
        strip_header "$V/boundary-$exe-final.fadv2d" "$V/boundary-$exe-final.bin"
done
step "boundary 13x11/3: native vs GFortran vs host (compare_2d_raw_fields.py)" "$LOGS/52-boundary-compare.log" \
    lf python "$FORMAL_ROOT/example/scripts/compare_2d_raw_fields.py" \
        "$V/boundary-input.fadv2d" "$V/boundary-native-final.fadv2d" "$V/boundary-gfortran-final.bin" \
        --cells 13,11 --steps 3 --evolution --cpu-final "$V/boundary-host-llvm-final.bin"

# 6. Evidence of native execution.
step "embedded GPU code (cuobjdump)" "$LOGS/60-cuobjdump.log" \
    "$CUDA_BIN/cuobjdump" --list-elf --list-ptx "$V/formal-native"
if [ "${SKIP_PROFILE:-0}" != 1 ]; then
    if [ -x "$CUDA_BIN/nsys" ]; then
        nsys_solver() { # nsys_solver NAME ARGS...
            local name="$1"
            shift
            (cd "$V" && "$CUDA_BIN/nsys" profile -o "nsys-$name" --force-overwrite true ./formal-native "$@") &&
            "$CUDA_BIN/nsys" stats --report cuda_gpu_kern_sum,cuda_gpu_mem_time_sum,cuda_api_sum \
                "$V/nsys-$name.nsys-rep"
        }
        step "nsys: workspace kernel launches" "$LOGS/61-nsys-workspace.log" \
            nsys_solver workspace "${WS_INPUT_ARGS[@]}"
        step "nsys: operators kernel launches" "$LOGS/62-nsys-operators.log" \
            nsys_solver operators "${OPS_ARGS[@]}"
    fi
    if [ -x "$CUDA_BIN/compute-sanitizer" ]; then
        for tool in memcheck racecheck synccheck initcheck; do
            step "compute-sanitizer $tool: workspace" "$LOGS/63-sanitizer-$tool-workspace.log" \
                "$CUDA_BIN/compute-sanitizer" --tool "$tool" --error-exitcode 99 "$V/formal-native" \
                    "${WS_INPUT_ARGS[@]}"
            step "compute-sanitizer $tool: operators" "$LOGS/63-sanitizer-$tool-operators.log" \
                "$CUDA_BIN/compute-sanitizer" --tool "$tool" --error-exitcode 99 "$V/formal-native" \
                    "${OPS_ARGS[@]}"
        done
    fi
fi

# 7. Benchmark: the workspace solver on a large grid, with LFortran's GPU
# loop transformations (the default, `all`) and without them (`none`).
# `solve_seconds` excludes compilation, setup, the warm-up step and I/O;
# the runs alternate between the two builds and the medians are reported.
benchmark() {
    local cfg i t
    for cfg in all none; do
        formal_fpm run --compiler "$LFORTRAN_COMPILER" --build-dir "$FORMAL_ROOT/build/bench-$cfg" \
            --flag "$NATIVE_FLAGS --gpu-loop-transforms=$cfg" \
            "${EXAMPLE[@]}" --runner cp -- "$V/formal-bench-$cfg" || return 1
        : > "$V/bench-$cfg.txt"
    done
    for ((i = 1; i <= BENCH_RUNS; i++)); do
        for cfg in all none; do
            t="$("$V/formal-bench-$cfg" --solver workspace --order 4 --cells "$BENCH_CELLS" \
                --steps "$BENCH_STEPS" --no-csv | awk '/^ *solve_seconds =/ {print $3}')"
            test -n "$t" || return 1
            echo "run $i, --gpu-loop-transforms=$cfg: solve_seconds = $t"
            echo "$t" >> "$V/bench-$cfg.txt"
        done
    done
    for cfg in all none; do
        echo "median solve_seconds, cells $BENCH_CELLS, $BENCH_STEPS steps, --gpu-loop-transforms=$cfg: $(sort -g "$V/bench-$cfg.txt" | awk '{a[NR]=$1} END {print (NR%2 ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2)}')"
    done
}
if [ "${SKIP_BENCHMARK:-0}" != 1 ]; then
    step "benchmark: workspace solver $BENCH_CELLS, $BENCH_STEPS steps" "$LOGS/65-benchmark.log" benchmark
fi

unset CUDA_DISABLE_PTX_JIT

# 8. LFortran's CUDA integration tests and the direct CUDA runtime API test
# on the real GPU.
if [ "${SKIP_LFORTRAN_TESTS:-0}" != 1 ]; then
    step "lfortran integration_tests -b cuda" "$LOGS/70-lfortran-cuda-tests.log" \
        lf integration_tests -b cuda -j"$JOBS"
    step "lfortran gpu_cuda_scratch_order" "$LOGS/71-lfortran-scratch-order.log" \
        lf env NVCC="$NVCC" CUDA_ARCH="$GPU_ARCH" \
            "$LFORTRAN_ROOT/src/tests/gpu_cuda_scratch_order.sh" "$WORK/scratch-order"
fi

echo
echo "Summary ($WORK):"
for line in "${SUMMARY[@]}"; do echo "  $line"; done
grep -a -h 'tests passed' "$LOGS"/10-*.log "$LOGS"/20-*.log "$LOGS"/70-*.log 2>/dev/null | sed 's/^/  /' || true
grep -a -h 'all three sequences passed' "$LOGS"/71-*.log 2>/dev/null | sed 's/^/  /' || true
grep -a -h 'median' "$LOGS"/65-*.log 2>/dev/null | sed 's/^/  /' || true
grep -a -h 'sm_' "$LOGS/60-cuobjdump.log" 2>/dev/null \
    | sed -E 's/.*\.(sm_[0-9a-z]+)\.(cubin|ptx)/\1 \2/' | sort | uniq -c | sed 's/^/  /' || true
exit "$FAILED"
