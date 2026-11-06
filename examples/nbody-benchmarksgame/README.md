# Benchmarks Game n-body

The [Benchmarks Game n-body benchmark](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/nbody.html)
simulates the Jovian planets for N steps and prints the initial and final
energy.

## Sources

| File | Origin |
|---|---|
| `nbody.ifx-5.f90` | Intel Fortran #5, as published (3-Clause BSD) |
| `nbody_opt.f90` | The same algorithm restructured after the C++ g++ #0 solution |
| `nbody.gpp-0.cpp` | C++ g++ #0 (3-Clause BSD) |

`nbody_opt.f90` stores the five bodies and the ten pairs as four-element
vectors, uses compile-time constant pair indices so the ten pairs unroll, and
runs each component loop over the four slots. The Rust #9 solution is also
measured below; its source is on the Benchmarks Game site and is not
committed here.

## Setup

- AMD Ryzen 9 9955HX (Zen 5), one pinned core (`taskset -c 17`), N =
  50,000,000, best of 6 interleaved runs on a shared machine.
- Compilers: gcc/g++ 15.3.0, rustc 1.99.1, flang 23.1.4, gfortran 16.2.0,
  LFortran `--fast` (LLVM 11.1.0).
- All binaries print the same two energies, `-0.169075164` and
  `-0.169087605`.

## Results

| Compiler / source | secs | vs C++ |
|---|---:|---:|
| **C++ g++ #0** | **0.956** | 1.00 |
| Rust #9 | 0.980 | 1.02 |
| **LFortran `--fast`, `nbody_opt.f90`** | **1.016** | **1.06** |
| LFortran `--fast`, `nbody.ifx-5.f90` | 1.413 | 1.48 |
| Flang `-O3 -march=native -ffast-math`, `nbody.ifx-5.f90` | 1.453 | 1.52 |
| Flang `-O3 -march=native -ffast-math`, `nbody_opt.f90` | 1.430 | 1.50 |
| GFortran `-O3 -march=native -ffast-math`, `nbody.ifx-5.f90` | 2.761 | 2.89 |
| GFortran `-O3 -march=native -ffast-math`, `nbody_opt.f90` | 2.243 | 2.35 |

LFortran is faster than Flang on both sources and is 6% behind the C++
solution. The LFortran numbers use #16838 (single precision reciprocal square
root), #16916 (block alloca hoisting) and #16917 (native CPU model). The
remaining gap to the C++ solution is tracked in #16918.

## Reproduce

```bash
g++ -O3 -fomit-frame-pointer -march=native -std=c++17 \
    nbody.gpp-0.cpp -o nbody.gpp0
rustc -C opt-level=3 -C target-cpu=native -C codegen-units=1 \
    nbody.rs -o nbody.rust9
flang -O3 -march=native -ffast-math nbody.ifx-5.f90 -o nbody.flang
flang -O3 -march=native -ffast-math nbody_opt.f90 -o nbody.flang-opt
lfortran --fast nbody.ifx-5.f90 -o nbody.lfortran
lfortran --fast nbody_opt.f90 -o nbody.lfortran-opt

taskset -c 17 ./nbody.gpp0 50000000
```
