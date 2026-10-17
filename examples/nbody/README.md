# N-body: single-core, OpenMP and Metal

Direct all-pairs gravitational N-body in single precision (`real(4)`), with
softened gravity:

    a_i = sum_j m_j (r_j - r_i) / (|r_j - r_i|^2 + eps^2)^(3/2)

Here G = 1, eps^2 = 0.01, and dt = 0.001. Kick-drift-kick leapfrog uses one
force evaluation per step. A portable 31-bit generator initializes every
implementation identically, with total mass approximately 1. This is a
throughput benchmark, not an astrophysical accuracy study or a replacement
for a faster asymptotic algorithm such as Barnes-Hut.

The same Fortran source runs on one CPU core, on host threads with OpenMP,
and on the GPU. The baselines are independently implemented, tuned C/NEON
and Metal versions of the tiled all-pairs algorithm described in
[GPU Gems 3, chapter 31](https://developer.nvidia.com/gpugems/gpugems3/part-v-physics-simulation/chapter-31-fast-n-body-simulation-cuda).
They are optimized baselines, not a claim of the fastest possible implementation.

## Run

The comparison harness requires an Apple Silicon Mac with Metal and Command
Line Tools. Build LFortran with Pixi first, then run from the repository root:

```bash
pixi run -e llvm11 bash examples/nbody/run.sh
```

It uses `build/llvm11/src/bin/lfortran`, Apple clang for C/Objective-C and
Metal framework linking, and Pixi's GFortran and OpenMP runtime. GFortran
must support Fortran 2018 `do concurrent` locality specifications (the
measurements below use GFortran 16.2). LFortran parallelizes `do concurrent`
with `--openmp`; GFortran needs `-ftree-parallelize-loops=<threads>`, not just
`-fopenmp`. The C multi-core baseline uses Grand Central Dispatch.

Defaults are **N=8192, 5 timed steps, 3 repetitions for every variant**.
`N_SINGLE`, `N_MULTI`, `N_GPU`, `STEPS_SINGLE`, `STEPS_MULTI`, `STEPS_GPU`,
`REPEAT` and `OMP_NUM_THREADS` can be overridden. N_GPU must be a multiple
of 128. Use identical sizes for direct speedup comparisons.

```bash
N_SINGLE=4096 N_MULTI=4096 N_GPU=4096 REPEAT=1 \
    pixi run -e llvm11 bash examples/nbody/run.sh
```

Every GPU program, including the peak microbenchmark, goes through
`gpu_run.sh`: one process at a time, a 20-second host-process timeout, and
a 4 GiB RSS limit. `GPU_TIMEOUT` accepts 1..30 seconds; `GPU_MAXMEM_MB`
changes the memory threshold. `GPU_RUN=/absolute/path/to/guard.sh` selects
another guard with the same command-prefix interface.

**A process timeout is not GPU cancellation.** Already submitted commands
may continue after their host exits, RSS does not cover every driver
allocation, and a long kernel can make the desktop unresponsive. Keep
individual launches short; do not use this example to probe GPU limits.
Doubling N quadruples force work. The small defaults intentionally do not
seek full GPU utilization.

Individual Fortran builds, inside the selected Pixi environment:

```bash
lfortran --fast examples/nbody/nbody.f90 -o /tmp/nbody_cpu
lfortran --fast --openmp --openmp-lib-dir="$CONDA_PREFIX/lib" \
    examples/nbody/nbody.f90 -o /tmp/nbody_omp
lfortran --fast --gpu=metal examples/nbody/nbody.f90 -o /tmp/nbody_gpu
bash examples/nbody/gpu_run.sh /tmp/nbody_gpu 8192 5
```

## Measurement and correctness

The conventional count is **20 flops per interaction**, as in GPU Gems and
the CUDA N-body sample:

    GFLOP/s = 20 * N^2 * timed_steps / seconds / 1e9

This includes self-interactions, whose force is zero. A reciprocal square
root counts as one flop by convention, not by its hardware cost. Consequently
the reported percentage of FP32 FMA peak is a useful comparison, **not a
measurement of hardware occupancy or utilization**.

The initial force evaluation and GPU kernel compilation are outside the
timed interval. Each program prints elapsed time, GFLOP/s, double-precision
total energy before/after, a mean-position checksum and a machine-readable
`RESULT` line. Timing includes force evaluation and position/velocity
updates, including the final device wait, but excludes energy computation.
Energy checking is also O(N^2) on the CPU.

The harness rejects missing/non-finite results, non-positive timings, energy
drift >= 1e-6 or a checksum differing from the two-Newton-step C baseline by
>= 1e-5 relative, in **every repetition**. These are trajectory smoke checks,
not an elementwise proof of force accuracy. The one-Newton-step C variant
is explicitly approximate and should not be treated as full-precision math.

## Results

Apple M4 Max (12 performance + 4 efficiency CPU cores, 40 GPU cores), macOS
15.8. LFortran 0.67.0-771, main `9e158cae68`, LLVM 11.1.0; Apple clang 17,
GFortran 16.2. Measurements refreshed with the small defaults after resuming
the work. Median of 3; all rows passed the trajectory checks. **This final
run overlapped an unrelated CPU build on the shared Mac** (load average
8--16). Treat it as a functional baseline under contention, not a controlled
performance result.

| Variant | Time [s] | GFLOP/s | % theoretical peak | % measured FMA peak |
|---|---:|---:|---:|---:|
| C single-core, NEON, 2 Newton steps | 0.1271 | 52.8 | 36.6% | 44.2% |
| C single-core, NEON, 1 Newton step (approximate) | 0.1128 | 59.5 | 41.2% | 49.8% |
| LFortran single-core | 0.5082 | 13.2 | 9.1% | 11.0% |
| GFortran single-core | 0.1550 | 43.3 | 30.0% | 36.2% |
| C multi-core, NEON/GCD, 2 Newton steps | 0.0324 | 207.1 | 11.2% | 12.0% |
| C multi-core, NEON/GCD, 1 Newton step (approximate) | 0.0290 | 231.4 | 12.5% | 13.4% |
| LFortran OpenMP | 0.1193 | 56.2 | 3.0% | 3.3% |
| GFortran multi-core | 0.0512 | 131.0 | 7.1% | 7.6% |
| Hand-written Metal | 0.0054 | 1233.2 | 7.6% | 8.2% |
| LFortran Metal | 0.0058 | 1165.9 | 7.2% | 7.8% |

All rows use N=8192 and 5 steps. These millisecond GPU timings include
submission overhead and clock ramp-up, so do not extrapolate their ratios
to sustained workloads. Even here LFortran Metal exceeds the tuned
multi-core C baseline, but asynchronous background load prevents a clean
speedup claim.

Earlier measurements, saved before the interrupted session and **not
rerun at large sizes**, illustrate why problem size matters:

| Target | N | Steps | LFortran GFLOP/s | Tuned baseline GFLOP/s |
|---|---:|---:|---:|---:|
| One core | 16384 | 10 | 17.4 | 60.6 (C, 2 Newton steps) |
| All cores | 32768 | 10 | 171.3 | 648.9 (C, 2 Newton steps) |
| GPU | 65536 | 40 | 5371.1 | 9461.6 (Metal) |

The last row is 33.2% versus 58.6% of the estimated GPU peak. It is archival
context, not a suggested default on a desktop machine.

### Peak denominators

Defaults describe this M4 Max, not every Apple GPU. Override `PEAK_SINGLE`,
`PEAK_ALL` and `PEAK_GPU` in GFLOP/s for other hardware.

| Target | Estimated theoretical FP32 peak | Measured FMA throughput in this run |
|---|---:|---:|
| One P-core | 144.4 GFLOP/s | 119.5 GFLOP/s |
| All CPU cores | 1849 GFLOP/s | 1726.6 GFLOP/s (sum of separate P/E group runs) |
| GPU | 16159 GFLOP/s | 15032.3 GFLOP/s |

GPU estimate: 40 cores x 128 FP32 lanes x 2 flops/FMA x 1.578 GHz. The
frequency is the highest state in this machine's IORegistry DVFS table,
not a guaranteed sustained clock. See
[metal-benchmarks](https://github.com/philipturner/metal-benchmarks)
for Apple GPU throughput background.

Single P-core estimate: 4 128-bit pipes x 4 FP32 lanes x 2 x 4.512 GHz.
All-core estimate: 12 x 32 x 3.852 GHz + 4 x 32 x 2.892 GHz. These are
frequency/pipe-model estimates, not Apple performance guarantees; see
[M4 core frequencies](https://eclecticlight.co/2026/04/13/cpu-core-frequencies-updated-for-all-current-apple-silicon-macs/)
and [cluster frequency behavior](https://eclecticlight.co/2024/12/02/inside-m4-chips-controlling-frequency/).
The CPU peak probe uses QoS hints, not hard core pinning. Its P- and E-group
results are measured separately, so their sum is not simultaneous all-core
throughput and can exceed the estimated denominator.

## Files and next optimizations

| File | Purpose |
|---|---|
| `nbody.f90` | SoA, `do concurrent` force/update loops, explicit iteration-local scalars |
| `c/nbody.c`, `c/common.h` | NEON baseline: 12-body register blocking, refined reciprocal square root, optional GCD |
| `metal/nbody.m`, `metal/nbody.metal` | Objective-C host, tiled Metal force kernel, float4 position/mass, fused update |
| `peak/` | CPU/GPU FP32 FMA throughput and reciprocal-square-root microbenchmarks |
| `run.sh`, `gpu_run.sh` | Comparison, validation and bounded GPU execution |
| `ablation.sh`, `metal/ablation.*` | Reproducible compiler-optimization experiments |
| [ANALYSIS.md](ANALYSIS.md) | Evidence, caveats and CPU/OpenMP/GPU optimization priorities |

This is a standalone example, not a new compiler regression or CI workload.
Build products and logs stay under ignored `build/`.
