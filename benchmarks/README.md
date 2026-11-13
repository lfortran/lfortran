# Benchmarks

Simple numerical kernels for measuring LFortran's compile time and the speed
of the generated code, with GFortran as the reference. The kernels are kept
small so that a regression is easy to track down.

| Kernel | What it does |
| --- | --- |
| `matmul_naive.f90` | dense matrix multiplication with explicit loops |
| `jacobi_2d.f90` | Jacobi iterations of a 5-point stencil |
| `fft_radix2.f90` | iterative radix-2 complex FFT, forward and inverse |
| `cg_sparse.f90` | conjugate gradient on a CSR sparse matrix |

Each kernel checks its own result and stops with an error if it is wrong.

## Running

Use a Release build of LFortran, otherwise the compile times are dominated by
the Debug build:

```
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DWITH_LLVM=ON
cmake --build build -j
benchmarks/run_benchmarks.py --lfortran build/src/bin/lfortran
```

This prints the best of 3 compile and run times for each kernel and the
LFortran/GFortran ratio. Useful options:

- `matmul_naive jacobi_2d`: run only these kernels
- `--lfortran-flags`, `--gfortran-flags`: change the flags (defaults: `--fast`
  and `-O3 -march=native -ffast-math -funroll-loops`)
- `-r N`: best of N runs
- `--json FILE`: also write the results as JSON
- `--no-gfortran`: only run LFortran

## Adding a kernel

Add a self-contained `.f90` program here that runs for at least a tenth of a
second, checks its result with `error stop`, and compiles with
`gfortran -std=f2018 -Wall -Wextra` without warnings. The script picks up every
`.f90` file in this directory.
