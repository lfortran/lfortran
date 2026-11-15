# Reallocation on assignment benchmark

Fortran 2003 requires `x = expr` to (re)allocate an allocatable array `x`
when it is unallocated or its shape differs from that of `expr`
("realloc-lhs"). Done naively, every such assignment pays for a shape check
and possibly a reallocation, even when the shapes already agree. This
benchmark measures that cost (see
[#10331](https://github.com/lfortran/lfortran/issues/10331)).

It is a timing benchmark, not a test: it is not part of CTest, the
integration tests or CI.

## Files

- `realloc_lhs.f90`: the kernels and a driver, `realloc_lhs <kernel> <n>
  <reps>`, which prints the time of the `reps` iterations and a checksum.
  Standard Fortran; it also compiles with GFortran.
- `realloc_lhs_ext.f90`: external procedures that some kernels call from
  their loops. It is compiled separately (`-c`) and linked, so that the
  compiler of `realloc_lhs.f90` can neither inline them nor see what they do.
  They are external procedures with interfaces in `realloc_lhs.f90`, not
  module procedures: LFortran compiles the procedures of a module into each
  object file that uses the module.
- `run_benchmark.py`: builds the program once per configuration, runs every
  kernel, checks the checksums and prints a Markdown table (Python 3
  standard library only).

## Kernels

Each kernel perturbs its input every iteration and adds an element of the
result to a checksum, so the assignment can be neither hoisted out of the
loop nor removed. Most run at a small `n` (the shape check dominates) and a
large `n` (the arithmetic dominates).

| kernel | assignment |
| --- | --- |
| `add` | `c = a + b` |
| `self` | `x = x + dt*v` (left-hand side also on the right) |
| `copy` | `c = a` |
| `ctor` | `c = [a(1) + 1, a(2)*2, a(3)]` |
| `func` | `c = f(a)`, explicit-shape function result |
| `allocfn` | `c = f(t, n)`, allocatable function result |
| `section` | `t = a(:,j) * 2`, 16 columns per iteration |
| `matmul` | `c = matmul(a, b)` |
| `add2d` | `c = a + b`, rank 2 (`n` x `n`) |
| `pack` | `c = pack(a, m)` with a fixed number of true mask elements |
| `comp` | `st%x = st%x + dt*st%v`, allocatable components |
| `dtexpr` | `q%x = p%x + 2*p%v`, component of another object |
| `inout` | `x = x + dt*v` in a routine with an `allocatable, intent(inout)` dummy, called from the loop |
| `stencil` | `un = u; un(2:n-1) = ...; u = un` |
| `append` * | `x = [x, v]`, growing to `n` elements, then reset |
| `local` * | `t = 2*a + 1` to an unallocated local inside a called routine |
| `resize` * | `x = a(1:m) + 1` with `m` changing every iteration |

The kernels below are written so that the compiler cannot remove the
shape check from the loop by proving it unnecessary or by moving it before
the loop, at least not without the transformation named in the last column:

| kernel | assignment | why the check stays in the hot path |
| --- | --- | --- |
| `xadd`, `xadd2d`, `xadd3d` | `c = a + b` (rank 1, 2, 3) in a procedure of `realloc_lhs_ext.f90` called from the loop | a check per call: the callee cannot know the allocation of its allocatable dummies |
| `xchain` | `t1 = a + b; t2 = t1*a; c = t2 - b` in an external procedure, with allocatable work arrays passed in | three checks per call |
| `xchainalloc` | the same with local work arrays allocated with `allocate` | the work arrays are allocated, but the shape of `c` is not known |
| `xchainlocal` * | the same with local work arrays that the assignments allocate | an allocation per work array and call |
| `opaque` | an opaque call, then `c = a + b` | the call may stop the program before the assignment (versioning) |
| `exit` | `if (s < 0) exit`, then `c = a + b` | the loop may leave before the assignment (versioning) |
| `target` | `c = a + b` to a TARGET dummy | a pointer may observe the reallocation (versioning) |
| `modvar` | `mc = a + b` to a module variable, with an opaque call in the loop | the call may change `mc` |
| `internal` | `c = a + b` to host variables in an internal procedure called from the loop | a check per call of the internal procedure |
| `saved` | `w = a + b` to a SAVE work array of a called procedure, allocated on the first call | a check per call |
| `pairs` | `d = p(j)%r - p(i)%r` for all pairs of 16 particles (3-vectors) | the shape is read from `p(j)`, which changes in every iteration |
| `pairsc` | `p(i)%d = p(j)%r - p(i)%r` | the target is a component of an array element |
| `concur` | `p(i)%d = p(i)%r + dt*p(i)%v` in DO CONCURRENT | one target per iteration |
| `jagged` | `out(i)%v = w(i)*rows(i)%v` over 16 rows | one target per iteration |
| `while` | `c = a + b` in a DO WHILE loop | (DO WHILE loops) |
| `cond` | `c = a + b` or `c = a - b` depending on the iteration | conditional assignments (versioning) |
| `chain` | `t1 = a + b; t2 = t1*a; c = t2 - b` through local work arrays | each shape is read from the previous work array (chained hoisting) |
| `datadep` | `c = a(1:k) + b(1:k)`, `k` read from data in every iteration | the shape depends on a value the loop changes |
| `alternate` * | `c = a(1:k) + b(1:k)`, `k` alternating between n and n - 1 | a reallocation in every iteration |
| `ctorv` | `c = [(2*a(j), j = 1, m)]` | the array constructor is a temporary of its own |

For `pairs`, `pairsc`, `concur` and `jagged` the table shows the time per
assignment (pair, particle or row), not per iteration of the outer loop.

Except for the kernels marked *, the left-hand side already has the right
shape, so they are also valid without reallocation on assignment and measure
the overhead of the shape checks. The kernels marked * require reallocation;
configurations that disable it show them as n/a.

## Running

From the repository root, after building LFortran (`pixi run -e llvm11
build`):

```bash
pixi run -e llvm11 bench_realloc_lhs > bench.md
```

The defaults compare `lfortran --fast --disable-realloc-lhs-arrays`,
`lfortran --fast --realloc-lhs-arrays` and, if `gfortran` is on `PATH`,
`gfortran -O3 -march=native` with `-fno-realloc-lhs` and `-frealloc-lhs`.
Configurations can be given explicitly as `label=compiler flags`, for
example to compare two LFortran builds in one table:

```bash
pixi run -e llvm11 bench_realloc_lhs \
    "main=/path/to/main/build/llvm11/src/bin/lfortran --fast --realloc-lhs-arrays" \
    "pr=lfortran --fast --realloc-lhs-arrays" \
    -o bench.md
```

Pass the realloc-lhs flag explicitly in every configuration (the script
warns otherwise), so that the numbers stay comparable when a compiler's
default changes. Other options (`--help` lists all):

- `-r N`: runs per kernel and configuration, the best is reported (default 5)
- `-k add,func:3`: only these kernels (`NAME` or `NAME:N`; `--list` lists them)
- `--scale 0.1`: scale the repetition counts, for a quick run
- `--cpu N`, `--no-pin`: the CPU the runs are pinned to with `taskset`
  (default: the last CPU available), or no pinning
- `--build-dir DIR`: keep the executables in `DIR`
- `-o FILE`: also write the table to `FILE`

The runs of all configurations are interleaved, so that a busy phase of a
shared machine affects them alike. Still, use an otherwise idle machine and
rerun kernels whose numbers look noisy (`-k`). On an idle machine, repeated
runs agree within about 2%; with all CPUs busy (including the pinned CPU's
hyperthread sibling), the small-`n` differences grew severalfold.

## Reading the table

Each cell is nanoseconds per iteration of the kernel's loop (for `section`,
one iteration does 16 assignments; for `pairs`, `pairsc`, `concur` and
`jagged`, per assignment), the best of `-r` runs. Every column after
the first also shows the change relative to the first configuration, so put
the baseline first: `+50.0%` means 1.5 times slower than the baseline. A
kernel the baseline cannot run (n/a) has no percentages. Below the table are
the exact commands and compiler versions.

The script exits with an error, after printing the table, if a run fails
(`FAIL` in the table) or if the checksums of a kernel differ between
configurations (beyond a relative tolerance, `--rtol`, default 1e-9, as
`--fast` allows reassociation). A difference means a miscompilation, not
noise.

A difference in a single kernel can also come from where the linker happens
to place its loop rather than from the generated code: `pack` at `n` = 1000
has measured 263 ns and 464 ns for identical machine code at two addresses.
Before reading such a difference as a regression, compare the code
(`--show-llvm`, `--show-asm`) or shift the placement, for example by adding
an unrelated procedure that the driver calls.
