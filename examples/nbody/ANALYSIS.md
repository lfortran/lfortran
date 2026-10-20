# N-body optimization opportunities

Measurements use LFortran main `9e158cae68`, LLVM 11, Apple M4 Max.
See [README.md](README.md#results) for the complete small-size comparison
and the distinction between throughput/peak ratios and GPU utilization.

| Target (N=8192, 5 steps) | LFortran GFLOP/s | Tuned baseline GFLOP/s | LFortran / baseline |
|---|---:|---:|---:|
| One core | 13.2 | 52.8 (C, 2 Newton steps) | 25% |
| All cores | 56.2 | 207.1 (C, 2 Newton steps) | 27% |
| Metal | 1165.9 | 1233.2 | 95% |

This final run overlapped another CPU build. Its GPU timings are too short
and its host-load interference too large to interpret the last column as
an optimization ceiling. Earlier, quieter small-size runs reached
1766.5 GFLOP/s with LFortran Metal and 4606.0 with the tuned kernel.

The energy drift is approximately 1.7e-10 and the position checksums agree
to the printed digits. This is short-trajectory validation, not proof that
the floating-point algorithms are identical. The approximate one-Newton-step
C variant is excluded from this accuracy-matched comparison.

`ablation.sh` reconstructs CPU binaries from LLVM IR and runs manually
modified Metal kernels. These are experiments, **not compiler fixes**.
Default sizes are small and every GPU execution is guarded. The CPU IR
experiment replaces the printer's invalid `void*` with `i8*`; the compiler
bug is listed below.

## One CPU core: make the inner reduction vectorizable

The generated inner loop is scalar, with `fsqrt` and `fdiv` per
interaction. LLVM 11's vectorizer reports both:

```
loop not vectorized: could not determine number of loop iterations
loop not vectorized: value that could not be identified as reduction is used outside the loop
```

Two changes together unblock it:

1. Give LLVM a provable iteration count. LFortran's lowered `do` increments
   a signed integer without no-overflow information. The experiment marks
   loop increments `nsw`; a production fix should preserve Fortran DO trip
   count/termination semantics in ASR rather than blindly adding flags to
   every integer operation.
2. Let LLVM recognize the reduction before FMA contraction. The ASR `fma`
   pass turns the accumulation into an `llvm.fma` call, which LLVM 11 does
   not recognize as a reduction. Skipping that pass leaves multiply/add
   operations LLVM can vectorize and contract under fast math.

Small-run ablation from an earlier, quieter run during this wrap-up, best of
3, N=8192 and 5 steps:

| CPU experiment | GFLOP/s |
|---|---:|
| Current code | 18.3 |
| Only loop-increment `nsw` | 17.1 |
| Only skip ASR `fma` | 14.4 |
| Both | 54.5 |

The last form is vectorized 4 lanes wide, interleave 2, approaching the
62.1 GFLOP/s C baseline measured in that earlier run, without NEON-specific
Fortran source.

Next investigate register blocking/unroll-and-jam of the independent outer
body loop. The C baseline handles 12 bodies per sweep over j, providing
independent arithmetic and reusing loads. Reciprocal-square-root estimates
with fewer Newton steps are a separate, explicitly approximate optimization,
not a prerequisite for vectorizing the original full-precision expression.
Alignment/alias information and CPU-model selection deserve investigation
too, but this experiment does not establish their payoff.

## OpenMP: vectorization, then load balance

The outlined force loop has the same scalar reduction as the single-core
version. `do concurrent` currently distributes equal contiguous chunks.
Equal work per thread need not be balanced on a hybrid P/E-core CPU.

| OpenMP experiment (same small workload, best of 3) | GFLOP/s |
|---|---:|
| Current code | 148.9 |
| Vectorizable reduction above | 452.7 |
| Plus `schedule(dynamic,16)` in an explicit OpenMP loop | 562.2 |

The immediate win is the shared vectorization fix. Scheduling should then
be measured across workloads and hardware before choosing a new default;
dynamic/guided scheduling has its own overhead. Keep that policy in ASR's
host-parallel lowering, not a backend heuristic.

## Metal: preserve useful loops and expose instruction-level parallelism

LFortran already keeps state on the GPU between launches and lowers the
outer concurrent loop to one GPU thread per body. The inner j loop is
serial per thread. The force and update are separate kernels.

Important source differences from the tuned baseline:

- A Fortran DO has become an increment-at-the-top `while` loop. A canonical
  counted loop can be easier for the Metal compiler to optimize.
- The j loop is not explicitly unrolled. The hand-written baseline uses a
  four-way unroll to expose independent loads/arithmetic.
- Launch planning chooses 256 threads/group; the baseline uses 128.
- The ASR FMA rewrite can produce a different contraction order from Metal's
  own fast-math compiler.

The freshly rerun N=8192, 5-step ablations are too short and noisy to rank
these transformations reliably: variants measured roughly 1.1--2.7 TFLOP/s,
while a separate benchmark run of the tuned kernel reached 4.6 TFLOP/s.
Submission cost and GPU clock ramp-up dominate this regime.

For context, the earlier session saved the following **larger** ablation
(N=65536, 40 steps, best of 3). It has not been rerun during the safe,
small-size wrap-up:

| Experiment | Group size | GFLOP/s |
|---|---:|---:|
| LFortran program, `--fast` | 256 | 5364.8 |
| Skip ASR `fma` | 256 | 5491.5 |
| Generated no-FMA force kernel in minimal host | 256 | 5813.3 |
| Same, different launch size | 128 | 6122.6 |
| Counted `for`, read-only inputs | 256 | 6178.2 |
| Plus unroll 4 | 256 | 8573.6 |
| Plus explicit `rsqrt` instead of `1/sqrt` | 256 | 8585.3 |
| Unroll 4, smaller groups | 128 | 9149.5 |
| Unroll 8, smaller groups | 128 | 8894.7 |
| Tiled through threadgroup memory, unroll 4 | 128 | 9235.5 |

These suggest, in order: inner-loop unrolling, canonical counted-loop
lowering, per-kernel launch planning, then contraction and runtime overhead.
They are evidence for experiments, not promises that the percentages transfer
to another kernel or compiler version. Reuse ASR loop transformations and
explicit GPU launch plans; keep backends mechanical.

Packing position/mass in float4 and fusing the force/update loop are
intentional source-level choices in the baseline, not compiler changes
proposed by this example. Threadgroup tiling was a small marginal gain in
the earlier large-N ablation; all lanes already share their reads of body j.

### Interpreting percent of peak

An interaction uses 12 ordinary FP32 arithmetic instructions and one
reciprocal square root when the six multiply/add pairs contract. If rsqrt
costs four FMA issue slots, that is approximately 16 slots for 20 counted
flops, versus 32 flops if every slot were an FMA. This gives a **rough
instruction-mix ceiling of 62.5% of FMA peak**, not a universal bound.
Issue overlap, instruction selection and the GPU generation can change it.
The included throughput probe measures rsqrt/FMA rates independently.
An N-body rate near 60% of FMA peak therefore need not mean low GPU
utilization; confirming utilization requires hardware counters.

## Compiler bugs encountered

These are unrelated to the example's successful benchmark variants and are
not fixed in this PR:

| Behavior | Report |
|---|---|
| BLOCK-local declaration in a concurrent loop fails OpenMP ASR verification | #12882 (same failure as the explicit OpenMP form) |
| `any(array_expression)` in a program containing a parallel region crashes the OpenMP pass | #12888 |
| `--show-asm` fails on a DO loop | #15223; use `-S` |
| `--fast --show-gpu-kernel-source` fails after FMA lowering | #14651; inspect the embedded kernel source instead |
| `--openmp --show-llvm` prints `void*` in the outlined thread-data type, rejected by LLVM 11 | #15367 |

The benchmark avoids these forms and declares all per-iteration scalar
temporaries with `local(...)`; unspecified scalar locality on OpenMP is
tracked separately in #15241.
