#!/usr/bin/env bash
# Builds and runs every variant of the N-body benchmark and prints the
# results table of README.md. From the repository root, after building
# LFortran with Pixi:
#
#     pixi run -e llvm11 bash examples/nbody/run.sh
#
# Environment overrides: LFORTRAN, GFORTRAN, CC, CXX, OPENMP_LIB_DIR, REPEAT,
# N_SINGLE, STEPS_SINGLE, N_MULTI, STEPS_MULTI, N_GPU, STEPS_GPU, GPU_TIMEOUT,
# GPU_RUN, and the theoretical peaks in GFLOP/s PEAK_SINGLE, PEAK_ALL,
# PEAK_GPU (see README.md for how the defaults, for the Apple M4 Max, are
# derived).
#
# The default problem sizes are small, so that the whole script takes about
# a minute and every GPU kernel launch takes milliseconds. Larger N gives the
# GPU more work and higher GFLOP/s, e.g.
#
#     N_SINGLE=8192 N_MULTI=8192 N_GPU=16384 STEPS_GPU=10 \
#         pixi run -e llvm11 bash examples/nbody/run.sh
#
# N_GPU must be a multiple of 128. A host-process timeout cannot cancel an
# already submitted kernel; keep individual launches short. GPU_RUN can
# select a different guard script.
set -euo pipefail

if [[ $(uname -s) != Darwin || $(uname -m) != arm64 ]]; then
    echo "the comparison harness requires an Apple Silicon Mac" >&2
    exit 1
fi

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
build=$here/build
mkdir -p "$build"

LFORTRAN=${LFORTRAN:-$root/build/llvm11/src/bin/lfortran}
GFORTRAN=${GFORTRAN:-$(command -v gfortran || true)}
OPENMP_LIB_DIR=${OPENMP_LIB_DIR:-${CONDA_PREFIX:+$CONDA_PREFIX/lib}}
# Apple clang compiles the C and Objective-C versions and is LFortran's linker
# (the Metal framework), so it goes first on PATH.
CC=${CC:-/usr/bin/clang}
CXX=${CXX:-/usr/bin/clang++}
export PATH=/usr/bin:$PATH
REPEAT=${REPEAT:-3}
N_SINGLE=${N_SINGLE:-8192};  STEPS_SINGLE=${STEPS_SINGLE:-5}
N_MULTI=${N_MULTI:-8192};    STEPS_MULTI=${STEPS_MULTI:-5}
N_GPU=${N_GPU:-8192};        STEPS_GPU=${STEPS_GPU:-5}
GPU_TIMEOUT=${GPU_TIMEOUT:-20}
export GPU_TIMEOUT
PEAK_SINGLE=${PEAK_SINGLE:-144.4}  # 4 pipes x 4 lanes x 2 x 4.512 GHz
PEAK_ALL=${PEAK_ALL:-1849}         # 12 P x 32 x 3.852 GHz + 4 E x 32 x 2.892 GHz
PEAK_GPU=${PEAK_GPU:-16159}        # 40 cores x 128 ALUs x 2 x 1.578 GHz
threads=$(sysctl -n hw.ncpu)
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-$threads}

for value in "$REPEAT" "$N_SINGLE" "$STEPS_SINGLE" "$N_MULTI" "$STEPS_MULTI" \
    "$N_GPU" "$STEPS_GPU" "$OMP_NUM_THREADS"; do
    if ! [[ $value =~ ^[1-9][0-9]*$ ]]; then
        echo "repeats, sizes, steps and thread count must be positive integers" >&2
        exit 2
    fi
done
if (( N_GPU % 128 != 0 )); then
    echo "N_GPU must be a multiple of 128" >&2
    exit 2
fi
gpu_run=(bash "${GPU_RUN:-$here/gpu_run.sh}")

echo "== building"
"$CC" -O3 -mcpu=native -ffast-math "$here/c/nbody.c" -o "$build/nbody_c"
"$CC" -O2 -fobjc-arc -framework Metal -framework Foundation \
    -DNBODY_METAL_SOURCE="\"$here/metal/nbody.metal\"" \
    "$here/metal/nbody.m" -o "$build/nbody_metal"
"$CC" -O2 "$here/peak/fma_cpu.c" -o "$build/fma_cpu"
"$CXX" -O2 -fobjc-arc -framework Metal -framework Foundation \
    "$here/peak/fma_metal.mm" -o "$build/fma_metal"
"$LFORTRAN" --fast "$here/nbody.f90" -o "$build/nbody_lfortran"
"$LFORTRAN" --fast --openmp ${OPENMP_LIB_DIR:+--openmp-lib-dir="$OPENMP_LIB_DIR"} \
    "$here/nbody.f90" -o "$build/nbody_lfortran_openmp"
"$LFORTRAN" --fast --gpu=metal "$here/nbody.f90" -o "$build/nbody_lfortran_metal"
if [ -n "$GFORTRAN" ]; then
    "$GFORTRAN" -O3 -mcpu=native -ffast-math "$here/nbody.f90" \
        -o "$build/nbody_gfortran"
    "$GFORTRAN" -O3 -mcpu=native -ffast-math \
        -ftree-parallelize-loops="$threads" "$here/nbody.f90" \
        -o "$build/nbody_gfortran_parallel"
fi

echo "== machine load"
uptime

echo "== measured peaks"
"$build/fma_cpu" | tee "$build/fma_cpu.log"
"${gpu_run[@]}" "$build/fma_metal" | tee "$build/fma_metal.log"
# The total GFLOP/s of a line of fma_cpu's output.
total() {
    awk -v k="$1" 'index($0, k) == 1 {print $(NF - 2)}' "$build/fma_cpu.log"
}
measured_single=$(total "1 performance core")
measured_all=$(echo "$(total "all performance cores")" \
    "$(total "efficiency cores")" | awk '{print $1 + $2}')
measured_gpu=$(awk '/^fma:/ {print $2}' "$build/fma_metal.log")
for value in "$PEAK_SINGLE" "$PEAK_ALL" "$PEAK_GPU" \
    "$measured_single" "$measured_all" "$measured_gpu"; do
    awk -v v="$value" 'BEGIN {exit !(v > 0)}' || {
        echo "missing or invalid peak measurement: $value" >&2
        exit 1
    }
done

# The median (by time) of REPEAT runs: "n steps time gflops drift checksum".
median_run() {
    local ref=$1 output sample
    shift
    : > "$build/samples.log"
    for sample in $(seq "$REPEAT"); do
        output=$("$@") || return $?
        printf '%s\n' "$output" >> "$build/samples.log"
    done
    python3 - "$build/samples.log" "$REPEAT" "$ref" <<'PY'
import math
import sys

rows = []
for line in open(sys.argv[1]):
    if line.startswith("RESULT "):
        fields = line.split()[1:]
        if len(fields) != 6 or not all(math.isfinite(float(v)) for v in fields):
            sys.exit("invalid RESULT line: " + line)
        if float(fields[2]) <= 0 or float(fields[3]) <= 0:
            sys.exit("non-positive timing or throughput: " + line)
        drift, checksum = map(float, fields[4:])
        if not (0 <= drift < 1e-6 and
                abs(checksum / float(sys.argv[3]) - 1) < 1e-5):
            sys.exit("correctness failure: " + line)
        rows.append(fields)
if len(rows) != int(sys.argv[2]):
    sys.exit("missing RESULT line(s)")
rows.sort(key=lambda row: float(row[2]))
print(" ".join(rows[(len(rows) - 1) // 2]))
PY
}

echo "== reference checksums (C, NEON)"
checksum() {
    local output
    output=$("$@") || return $?
    printf '%s\n' "$output" | awk '/^RESULT/ {found++; print $7}
        END {if (found != 1) exit 1}'
}
ref_single=$(checksum "$build/nbody_c" "$N_SINGLE" "$STEPS_SINGLE")
ref_multi=$(checksum "$build/nbody_c" "$N_MULTI" "$STEPS_MULTI" all)
ref_gpu=$(checksum "$build/nbody_c" "$N_GPU" "$STEPS_GPU" all)

rows=()
# run name peak measured_peak reference_checksum command...
run() {
    local name=$1 peak=$2 measured=$3 ref=$4
    shift 4
    echo "-- $name: $*" >&2
    local result
    result=$(median_run "$ref" "$@") || return $?
    cp "$build/samples.log" "$build/${name// /_}.log"
    echo "$result" | awk -v ref="$ref" '{
        diff = ($6 - ref) / ref; if (diff < 0) diff = -diff
        if (!(diff < 1e-5 && $5 >= 0 && $5 < 1e-6)) exit 1
    }' || {
        echo "correctness failure for $name: $result; reference checksum $ref" >&2
        return 1
    }
    rows+=("$(echo "$result" | awk -v name="$name" -v peak="$peak" \
        -v measured="$measured" -v ref="$ref" '{
        diff = ($6 - ref) / ref; if (diff < 0) diff = -diff
        ok = (diff < 1e-5 && $5 < 1e-6) ? "ok" : "FAIL"
        printf "| %s | %d | %d | %.4f | %.1f | %.1f%% | %.1f%% |", name, \
            $1, $2, $3, $4, 100 * $4 / peak, 100 * $4 / measured
        printf " %s (drift %s, checksum diff %.1e) |\n", ok, $5, diff
    }')")
}

single() {
    run "$1" "$PEAK_SINGLE" "$measured_single" "$ref_single" "$build/$2" \
        "$N_SINGLE" "$STEPS_SINGLE" "${@:3}"
}
multi() {
    run "$1" "$PEAK_ALL" "$measured_all" "$ref_multi" "$build/$2" \
        "$N_MULTI" "$STEPS_MULTI" "${@:3}"
}
gpu() {
    run "$1" "$PEAK_GPU" "$measured_gpu" "$ref_gpu" "${gpu_run[@]}" \
        "$build/$2" "$N_GPU" "$STEPS_GPU"
}

echo "== benchmarks (median of $REPEAT runs)"
single "C single-core (NEON)" nbody_c
single "C single-core (NEON, 1 Newton step)" nbody_c 1 1
single "LFortran single-core" nbody_lfortran
[ -n "$GFORTRAN" ] && single "GFortran single-core" nbody_gfortran
multi "C multi-core (NEON, GCD)" nbody_c all
multi "C multi-core (NEON, GCD, 1 Newton step)" nbody_c all 1
multi "LFortran OpenMP" nbody_lfortran_openmp
[ -n "$GFORTRAN" ] && multi "GFortran multi-core" nbody_gfortran_parallel
gpu "Metal hand-written" nbody_metal
gpu "LFortran Metal" nbody_lfortran_metal

echo
echo "Peaks [GFLOP/s]: theoretical single-core $PEAK_SINGLE," \
    "all-core $PEAK_ALL, GPU $PEAK_GPU;"
echo "measured single-core $measured_single, all-core $measured_all," \
    "GPU $measured_gpu."
echo
echo "| variant | N | steps | time [s] | GFLOP/s |" \
    "% of theoretical peak | % of measured peak | correctness |"
echo "|---|---:|---:|---:|---:|---:|---:|---|"
printf '%s\n' "${rows[@]}"
