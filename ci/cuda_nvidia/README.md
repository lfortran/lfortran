# Running LFortran's CUDA backend on NVIDIA GPUs

This directory holds the recipe used to validate `--gpu=cuda` on a real NVIDIA
GPU: LFortran's own GPU integration tests, and the BerkeleyLab
[Formal](https://github.com/BerkeleyLab/formal) library, its test suite and its
2D advection-diffusion example, which is also benchmarked. CI has no NVIDIA
GPU (it runs the same device code on the CPU with `--gpu=cuda_cpu`), so this
validation is manual: run it on an NVIDIA machine after changing GPU code.
`run_formal_cuda.sh` performs every step below and prints a summary; the
sections explain what it does and how to run the pieces by hand.

Validated on: NVIDIA RTX PRO 6000 Blackwell Server Edition (compute capability
12.0, `sm_120`), driver 610.43.02, CUDA 13.3, Ubuntu 24.04, Pixi `llvm11`
environment (LLVM 11.1.0, conda-forge GCC 15.3 as nvcc's host compiler).

## Prerequisites

- An NVIDIA driver and a CUDA toolkit with `nvcc` on `PATH`
  (e.g. `export PATH=/usr/local/cuda/bin:$PATH`). `cuobjdump`, `nsys` and
  `compute-sanitizer` from the same toolkit are used for evidence when present.
- Git and [Pixi](https://pixi.sh). Everything else (LLVM 11, the host C/C++
  compilers, FPM, GFortran 16) comes from Pixi environments. No root access is
  needed.

## 1. Build LFortran

From the root of this checkout:

```bash
CMAKE_BUILD_PARALLEL_LEVEL=16 pixi run -e llvm11 build \
    '-DCMAKE_CXX_FLAGS_DEBUG=-g -O2' '-DCMAKE_C_FLAGS_DEBUG=-g -O2' > build.log 2>&1
pixi run -e llvm11 bash -c 'command -v lfortran; lfortran --version; command -v nvcc'
```

`lfortran` must resolve to `build/llvm11/src/bin/lfortran` of this checkout.

## 2. Quick check

```bash
cat > saxpy.f90 <<'EOF'
program saxpy
implicit none
integer, parameter :: n = 10000000
real, allocatable :: x(:), y(:)
real :: a
integer :: i
allocate(x(n), y(n))
a = 2.0; x = 1.0; y = 3.0
do concurrent (i = 1:n)
    y(i) = a*x(i) + y(i)
end do
if (abs(y(1) - 5.0) > 1e-6 .or. abs(y(n) - 5.0) > 1e-6) error stop "wrong result"
print *, "OK"
end program
EOF
pixi run -e llvm11 lfortran --gpu=cuda saxpy.f90 -o saxpy_gpu
CUDA_DISABLE_PTX_JIT=1 ./saxpy_gpu          # prints OK
cuobjdump --list-elf --list-ptx saxpy_gpu   # cubins for the local GPU (sm_120 here), no PTX
```

## GPU options

Every `do concurrent` loop is offloaded when a GPU backend is selected; see
[GPU offloading](../../doc/src/gpu_offloading.md) for how a loop is lowered and
[GPU loop optimization passes](../../doc/src/gpu_optimization_passes.md) for
the loop transformations.

- `--gpu=cuda` compiles the device code with nvcc and runs it on the GPU.
  `--gpu=cuda_cpu` compiles the same device code as host C++ and runs it on
  the CPU (no GPU needed; this is what CI runs). `--gpu=metal` targets Apple
  GPUs.
- `--gpu-arch=<arch>` selects the GPU architecture for `--gpu=cuda`, e.g.
  `sm_120`. The default is `native`, the GPUs of the compiling machine. On a
  machine without a GPU nvcc then falls back to its oldest default
  architecture with a warning, so pass an explicit architecture when
  cross-compiling.
- `--device-compiler=<path>` selects the driver that compiles and links the
  device code: nvcc for `cuda` (the default), the host C++ compiler for
  `cuda_cpu`.
- `--show-gpu-kernel-source` prints the generated kernel source (`.cu` for
  CUDA) and exits.
- `--gpu-allow-cpu-fallback` runs a loop that uses a construct the GPU does
  not support (e.g. `real(10)`, `real(16)` or I/O) on the CPU with a warning,
  instead of failing compilation.
- `--gpu-offload-omp-loops` also offloads `!$omp parallel do` loops.
- `--gpu-loop-transforms=<list>` selects the loop transformations applied to
  device loops before offloading (`cuda` and `cuda_cpu` only): `all` (the
  default), `none`, or a comma separated list of `fuse`, `swap`, `split`,
  `jam` and `skew`.
- `--gpu-cache-bytes=<N>` states the size of the device's last-level cache
  that the transformations assume (default: the L2 size the device reports at
  run time).
- `--gpu-split-min-iterations=<N>` is the fewest iterations of a loop that
  index-set splitting splits at run time (default 131072; 0 splits every loop
  that has an interior).
- `CUDA_DISABLE_PTX_JIT=1` at run time makes a binary built for the wrong
  architecture fail instead of being JIT compiled by the driver.

## 3. LFortran GPU integration tests on the GPU

```bash
CUDA_DISABLE_PTX_JIT=1 pixi run -e llvm11 integration_tests -b cuda -j16 > cuda.log 2>&1
grep 'tests passed' cuda.log
```

This runs every integration test labeled `cuda` (the tests labeled `cuda_cpu`
that also pass on a real GPU, plus native-only tests such as
`gpu_cuda_scratch_order`). CI runs only `-b cuda_cpu`.

## 4. Formal

```bash
ci/cuda_nvidia/run_formal_cuda.sh /path/to/work-dir > formal.log 2>&1
tail -50 formal.log
```

The script writes only to the work directory (besides the Pixi environments
and LFortran's integration test build directory). It clones Formal at the
validated revision (`FORMAL_REF`, the head of
[BerkeleyLab/formal#71](https://github.com/BerkeleyLab/formal/pull/71), which
contains #70 and provides the workspace solver, raw input/output and the
comparison scripts; neither is merged into Formal's `main` yet), then, with
logs in `<work-dir>/logs` and data in `<work-dir>/validation`:

1. Builds and runs Formal's FPM test suite and both solvers with
   `--gpu=cuda_cpu` (expect 32/32 tests, 0 skipped).
2. Builds all of Formal (library, tests, every example) with
   `--gpu-allow-cpu-fallback` and checks that no loop is reported as running
   on the CPU, i.e. every `do concurrent` is offloaded.
3. The same tests and solvers with native `--gpu=cuda --gpu-arch=native` and
   `CUDA_DISABLE_PTX_JIT=1`, in a separate build directory.
4. Builds three executables of the example: native GPU, ordinary host LLVM,
   and GFortran 16 as an independent oracle.
5. Compares full fields from a shared raw input:
   - a 17x13, 9-step Gaussian case, native against host LLVM, with Formal's
     `validate_2d_advection_workspace.py`;
   - a nonsquare 13x11, 3-step case with nonzero, non-smooth boundaries
     (`make_boundary_input.py`), native against GFortran and host LLVM, with
     Formal's strict `compare_2d_raw_fields.py` (finite fields, evolution,
     unchanged boundaries, default tolerances).
6. Records evidence of native execution: `cuobjdump` of the executable, `nsys`
   kernel/transfer summaries, and `compute-sanitizer` memcheck, racecheck,
   synccheck and initcheck on both solvers.
7. Benchmarks the workspace solver on a 2048x2048 grid for 1000 steps, built
   with the default `--gpu-loop-transforms=all` and with `none`, alternating
   the two for 5 runs each, and reports the median `solve_seconds` (which
   excludes compilation, setup, the warm-up step and I/O, and includes copying
   the field to the device and back).
8. Runs `integration_tests -b cuda` (section 3).

Environment overrides: `GPU_ARCH` (e.g. `sm_120`), `JOBS`, `FORMAL_REPO`,
`FORMAL_REF`, `BENCH_CELLS`, `BENCH_STEPS`, `BENCH_RUNS`, `SKIP_PROFILE=1`,
`SKIP_BENCHMARK=1`, `SKIP_LFORTRAN_TESTS=1`.

Notes on the recipe:

- Keep all Fortran flags in a single FPM `--flag` argument.
- `--input` supplies the grid, step count and time step, so Formal rejects it
  together with `--cells`, `--steps` or `--dt`; and only the workspace solver
  accepts `--input`.
- `formal/benchmarks/pixi.toml` only lists `osx-arm64`; `fpm/pixi.toml` here
  pins the same FPM (0.13.0) for Linux.
- Formal needs GFortran 16 (its README asks for 16.3+); conda-forge's 16.2
  builds it with `-fcoarray=single`. `gfortran16/pixi.toml` provides it.
- `nsys` and `compute-sanitizer` need a writable `TMPDIR`; the script uses
  `<work-dir>/tmp`, since a shared `/tmp/nvidia` may not be writable.
- The operators solver cannot read a raw input, so its initial field is
  computed by each build. GPU `sinf`/`expf` and the host math libraries round
  differently (host LLVM and GFortran differ as well), at the 1e-7 level, so
  that path is compared against its own initial state rather than bitwise
  across builds.

## Results

Measured with LFortran `main` at 8349ea3d00 and Formal at
`ef4fb6e1cfb6eed69c588e1e92fbf739ebcb09bf`, on the machine in "Validated on"
above:

| Check | Result |
|---|---|
| Formal tests, `cuda_cpu` | 32/32, 0 skipped |
| Formal tests, native CUDA, PTX JIT disabled | 32/32, 0 skipped |
| `do concurrent` in Formal offloaded | 65 of 65 (no loop reported as running on the CPU) |
| Embedded device code | 14 `sm_120` cubins, no PTX |
| Gaussian 17x13/9, native vs host LLVM | max abs 1.2e-10, relative L2 7.9e-11 |
| Boundary 13x11/3, native vs GFortran 16 vs host LLVM | bitwise identical; boundaries fixed; field evolves |
| compute-sanitizer (4 tools x 2 solvers) | 0 errors |
| nsys, workspace solver (9 steps) | 52 kernel launches, 8 host-to-device and 14 device-to-host copies |
| Benchmark, workspace solver 2048x2048, 1000 steps | median `solve_seconds` 0.12 s (`--gpu-loop-transforms=all`), 0.56 s (`none`): 4.5x |
| `integration_tests -b cuda`, PTX JIT disabled | 102 of 105 pass; the 3 `gpu_cuda_scratch_order*` tests fail, see below |

Known issues:

- The native-only `gpu_cuda_scratch_order*` tests are compiled by CMake for
  nvcc's default `sm_75`, not for the local GPU, so on newer GPUs they only
  run through the PTX JIT and fail with `CUDA_DISABLE_PTX_JIT=1`; they pass
  without it
  ([#15821](https://github.com/lfortran/lfortran/issues/15821)).
- The operators solver runs every kernel on the GPU, but LFortran
  synchronizes at each procedure exit and re-uploads data per launch (1185
  host-to-device copies in a 2-step run), and part of its arithmetic is host
  array expressions, so most of its time is host work and transfers. The workspace
  solver is fully device resident. See
  [#15581](https://github.com/lfortran/lfortran/issues/15581).
