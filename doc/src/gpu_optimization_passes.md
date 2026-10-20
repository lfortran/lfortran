# GPU Loop Optimization Passes

The passes described here transform the parallel loops (`do concurrent`)
that the dispatch has assigned to the device, before they are turned into
kernels (see [GPU offloading](gpu_offloading.md)). They are general loop
transformations of the kind a stencil or polyhedral compiler makes; none of
them knows anything about a particular application. Each proves that what it
does preserves the meaning of the program, and leaves the loops as they are
when it cannot.

They run for `--gpu=cuda` and `--gpu=cuda_cpu` (on by default), right after
the dispatch has assigned the loops (the pass `gpu_loop_transforms`, which
runs `gpu_loop_fusion`, `gpu_time_skew`, then `gpu_index_set_split`, before
`gpu_offload`; each of the three can also be run on its own with `--pass`),
so their output is ordinary `do concurrent` nests and host code that the
rest of the offload pipeline lowers as it lowers any other; Metal keeps its
loops as written. `--gpu-loop-transforms` selects them: `all` (the
default), `none`, or a comma separated list of `fuse`, `swap`, `split`,
`jam` and `skew` (`swap` only applies to loops that are fused); any other
value is an error. The
choices that depend on the size of the device's last-level cache ask the
runtime for it (`lfortran_gpu_cache_bytes`, the L2 size on CUDA) unless
`--gpu-cache-bytes=N` states it (the tests use a small value to exercise
the paths for large problems on small ones).

The measurements quoted below are of Formal's 2D advection-diffusion
example (`--solver workspace --order 4`, the `split` kernel: per Runge-Kutta
stage one loop computing four face flux arrays and one computing their
divergence and the stage update), 1000 steps, on an NVIDIA RTX PRO 6000
Blackwell (sm_120).

## Affine analysis (`gpu_affine.h`)

Subscripts and conditions are turned into affine forms over *atoms*: loop
indices, and values that do not change while the loops run (scalars the
loops do not assign, bounds of arrays, expressions of those). A condition
becomes a disjunction of conjunctions of constraints `e >= 0` for the value
it has on a path. Implications between constraints are decided by
Fourier-Motzkin elimination with integer tightening; anything the analysis
cannot express gives no constraint, so it can only fail to prove, never
prove something false.

## Inlining (`gpu_loop_utils.h`)

A call of a pure function with a scalar result can be inlined into a loop
body. The statement holding the call becomes a `block` whose variables hold
the function's result and locals, so they are private to an iteration. A
scalar dummy argument stands for the actual argument where that does not
change what is evaluated, and an assumed-shape array dummy for the actual
array, with its subscripts shifted by the difference of the lower bounds.
Functions with `return`, input/output, subroutine calls, local arrays or
saved locals are not inlined, nor calls in an operand that need not be
evaluated (the second operand of a logical operator, an argument of
`merge`).

## Producer-consumer fusion (`fuse`, `gpu_loop_fusion.cpp`)

Two adjacent parallel loops, a producer that assigns arrays element by
element and a consumer that reads them at constant offsets, become one loop
over the consumer's iterations in which each element the consumer reads is
the producer's expression evaluated at its subscripts. The intermediate
arrays are then neither stored nor loaded, and the producer's launch goes.

Legality:

* The producer assigns each array once, element by element at its own
  iteration, possibly under `if` tests (without `else`); its expressions
  read neither its own arrays nor what the consumer assigns (except arrays
  that are renamed, below), nor its private variables.
* The intermediate and renamed arrays are not pointers, nor components
  reached through a pointer, nor variables that may be read or written
  under another name (a `target`, a variable in a common block, a
  `volatile` one, or one named where the program takes an address or
  associates a pointer, which covers `equivalence`), since such reads
  would not be seen.
* Every function the producer calls is pure and *self-contained*: it names
  no variable but its own dummy arguments and locals (named constants
  aside), and calls only functions that are self-contained too, so it reads
  nothing but what its actual arguments name. The consumer only reads the
  intermediate arrays, element by element at affine subscripts; pure
  functions it passes them to are inlined first, and any other function it
  calls must be self-contained.
* Every element the consumer reads is one the producer writes: on every
  path through the consumer (its loop bounds and the `if` tests around the
  read) the subscripts lie in the producer's loop bounds and satisfy its
  tests, which must be expressible exactly (a test whose normal form has
  more than 64 alternatives is not). A read is inside the bounds of the
  array, as in any valid program; when coverage depends on how an
  intermediate array was allocated, the fused loop runs only when a
  run-time check finds the array's bounds within the producer's loop bounds
  and the bounds its tests put on single indices.
* An array the consumer writes and the producer reads (the stage update of a
  field whose fluxes are computed from it) would be read and written by
  different iterations of the fused loop. The consumer writes a copy of it
  instead, which is copied back after the fused loop. The consumer must
  write the same element, at its own iteration, on every path, and may read
  that element only before writing it. The array is allocatable, and the
  `do` loops below hold nothing that may change its allocation (the copy is
  allocated once, before them): their statements define neither the array
  as a whole nor the variable it is a component of, whether by an
  assignment (of a derived-type object too) or as an actual argument of a
  subroutine or a function, and their effects are ones the pass models
  (below; `allocate`, for example, is not).
* The intermediate arrays keep the values the producer gave them, which the
  program may read later. The two loops must be the body of `do` loops (time
  steps, stages), taken from the innermost outwards while each has a unit
  step and bounds without function calls, and is not left early (`exit`,
  `cycle`, `return`, `go to`). Its other statements, including those nested
  in `block`, `if`, `select case` and `do` constructs, must neither read nor
  write the intermediate arrays and must call only self-contained
  procedures. Their effects must be ones the pass models; any other
  statement or expression ends the chain. What they may define is computed
  by one walker that classifies every statement and expression by its kind:
  * assignments, `do` variables and internal files written define what they
    name;
  * a call of a pure procedure, or of a self-contained function without
    input/output (impure ones included), defines each actual argument whose
    dummy is not `intent(in)` or `value`; a pointer dummy, a type-bound or
    procedure-pointer call and a call of any other procedure (an impure
    subroutine, for example) are unknown;
  * `print` and `write` without specifiers that define something define
    nothing; `iostat=`, `iomsg=`, `id=`, a namelist, user-defined output,
    asynchronous output and an implied `do` in an output list are unknown;
  * every other kind (input, `allocate`, pointer association, `associate`,
    intrinsic subroutines, ...) is unknown.

  They must assign neither the loop's variable nor what the producer's
  bounds and tests read. The producer's bounds and tests must not read the
  loop's variable, nor a variable that may change through another name:
  a pointer, a `target`, a variable in a common block, a `volatile` one, or
  one named where the program takes an address or associates a pointer, on
  either side (this covers `equivalence`, in a procedure or a module). The
  bounds of the `do` loops nested in it must read neither its variable nor
  anything assigned in it nor such a variable, so that they run the same
  iterations in each of its iterations (in particular, at least one in its
  last iteration whenever they do in any). Then only the last iteration's
  values can be read later: the original loops run in that iteration and
  the fused loop in all others. The last iteration is recognized by
  comparing the loop's variable with the loop's end, which is copied into a
  variable right before the loop, as the loop itself evaluates it once when
  it starts; the statements of the loop may therefore change what the end
  reads, and assign it. Without such loops nothing is fused.

The original loops, which run only in the last iteration, are left as
written by the passes after this one (index-set splitting would split them
too; `gpu_loop_transforms` hands the list of them from the fusion to the
splitting): the offload pipeline then still sees that the producer assigns its
arrays whole, so they are not copied to the device before it runs (4 ms at
2048², 17 MB per array).

Cost model: an element is computed by every iteration that reads it. The
fusion is made when that extra arithmetic, the producer's expression with
the bodies of the functions it calls times the reads beyond the first, is at
most 64 operations per byte of memory traffic removed (a store and a load
per element), the order of a GPU's ratio of arithmetic to memory
throughput.

Effect (2048², 1000 steps): 0.544 s to 0.454 s. The fused stage kernel takes
95 µs, the copy of the renamed field 10 µs, against 137 µs for the two
original kernels.

When inlining has shifted the subscripts of an intermediate array by its
lower bounds, the fused loop also requires (at run time) each lower bound to
be the producer's first index, and uses that value instead of reading the
bound: the subscripts become constant offsets of the loop indices, which the
index-set splitting below can reason about.

## Index-set splitting (`split`, `gpu_index_set_split.cpp`)

A stencil evaluates different formulas near the boundaries of its domain:
its body, often the bodies of the pure functions it calls, tests the loop
indices against the bounds (`if (i == 1) ... else if (i + 2 < n) ...`). Every
iteration runs the tests, and the compiler cannot share loads between the
formulas of neighbouring faces or of different quantities, which sit in
different branches. Far from the boundaries every such test has one value.

The pass inlines the pure functions the body calls, collects the tests that
compare one loop index with an expression differing from the loop's lower or
upper bound by a small constant (up to 16), and takes for each dimension the
fewest iterations at each end outside of which all of them have one value
(shrunk as long as no test that had one value stops having it). The nest
becomes:

* the interior, where every test the affine analysis proves to have one
  value is replaced by the branch it takes, giving straight-line code;
* the frame, the iterations outside the interior, numbered by one index and
  mapped back to the original ones (bands of whole rows at both ends of the
  outer dimension, then strips at both ends of the inner one), running the
  original body.

The frame is not a launch of its own: its iterations are few and each runs
the long general body, so on its own it leaves the device mostly idle for as
long as one of them takes (8 µs at 2048²). They are packed as extra rows in
front of the interior's outer dimension instead, so they start first and run
alongside the interior, in the same launch. The nest keeps the original loop
indices for the interior; the offload pipeline still picks the contiguous
index of the launch from the interior's stores (a store whose subscript
names no loop index, such as the frame's, does not decide it).

The frame's computed indices also hide from the offload that the iterations
assign every element of an array before reading it, which lets a launch skip
copying the array to the device (write-discard, see
[GPU offloading](gpu_offloading.md)). The split nests run the iterations and
assignments of the original one, so the pass works this out on the original
nest and records it on the split nests as an `OMPOverwrite` clause, with the
condition (the loop runs over exactly the array's bounds) the launch then
evaluates. A pass that later changes which iterations a nest runs drops the
clause.

A domain too small to have an interior runs the original nest. Nests of one
and two dimensions are split, and only bodies that compute values of
intrinsic scalar types (assignments of scalars and array elements, `if`,
`select case`, `do`, `block`, calls of pure functions with scalar results);
the offload pipeline analyses other constructs on the loop as written. Tests
that the analysis cannot decide stay in the interior as they are.
A parallel loop in a procedure that a parallel loop calls runs on one
thread, one iteration after the other, so such procedures are not split.

A small domain runs the original nest too. Its launch takes as long as its
slowest thread, which in the frame first computes its indices and then runs
the original body, so the split nest only takes longer: on an NVIDIA RTX PRO
6000 the split stencil kernel of a 256² domain takes 1.15 µs instead of
0.99 µs, and that of a 512² one 1.15 µs instead of 1.44 µs. The host splits
only a nest of at least 131072 iterations (`--gpu-split-min-iterations=N`; 0
splits every nest that has an interior).

Effect (2048², after fusion): 0.454 s to 0.219 s with the frame in a launch
of its own, and from 0.170 s to 0.149 s (after the ping-pong buffers below)
by packing the frame into the interior's launch. The fused stage kernel
goes from 95 µs to 25.5 µs (interior) plus 8.2 µs (frame): with the boundary
rows gone the diffusive and advective fluxes of a face share their loads,
and the remaining loads and arithmetic are those of the interior formulas.

## Ping-pong buffers (`swap`, part of `gpu_loop_fusion.cpp`)

The copy of a renamed array is copied back after every fused loop unless it
can take turns with the array: the fused loop then reads one of the two and
writes the other, which one alternating from one execution to the next (a
host flag picks between two copies of the fused loop), and nothing is copied.
This is valid when the copy starts equal to the array (a device loop copies
it once, before the `do` loops the fusion relies on), the two keep agreeing
on every element the consumer does not write, and nothing looks at the array
between two fused executions: no other statement in those loops reads or
writes the array, and the consumer writes the same elements in every
execution (its bounds read neither the variables of those loops nor
anything assigned in them nor a variable that may change through another
name). The
original loops, run in the last iteration, first copy back what the latest
execution left in the copy, if it is there. Exchanging the allocations with
`move_alloc` would do the same, but the host statements around a move make
the offload pipeline wait for the device.

Effect (2048²): 0.217 s to 0.170 s (the 10 µs copy per stage goes).

## Unroll-and-jam of the interior (`jam`, part of `gpu_index_set_split.cpp`)

The interior of a split nest of two dimensions runs several consecutive
iterations of its outer dimension (the non-contiguous one) in one thread,
one after the other. The iterations of a parallel loop are independent, so
any of them may run in one thread in any order; run one after the other in
straight-line code, what neighbouring rows read and compute in common (the
loads of the rows between them, and for a stencil the fluxes of the faces
they share) is loaded and computed once by the device compiler (the kernel
buffers do not alias, so the stores of one row do not force the next row
to reload). The interior's rows are a multiple of the factor, and the rows
left over join the frame.

The factor is three when the body reads some array at two or more offsets
along the outer dimension, and one otherwise. The outer dimension must not be
the contiguous one: every array element the body stores must have the inner
index as its first subscript, so that the offload makes the inner index
contiguous across the threads of a launch (jamming the contiguous index would
spread the accesses of neighbouring threads apart). With the fourth-order stencil
three rows compute 6 instead of 12 y faces; two and four rows were slower
(more instructions per cell with two, register pressure with four).

The jam pays only while what a launch touches stays in the device's cache,
where the copies share loads and arithmetic. When the data streams from
memory it does not reduce the memory traffic, and its registers lower the
occupancy the memory throughput needs (at 4096² the jammed stage kernel
reaches 1.22 TB/s of DRAM bandwidth at 60% occupancy, the plain one 1.36
TB/s at 70%). Both nests are generated, and the host picks the jammed one
when the launch's iterations times the bytes an iteration names (one
element of every array of the body) fit in the last-level cache, which the
runtime reports (`lfortran_gpu_cache_bytes`, the L2 size on CUDA).

Effect (2048²): 0.149 s to 0.133 s. The stage kernel goes from 29 µs to
24.7 µs, with 37% fewer instructions; it now runs at the L2 bandwidth
(5.8 TB/s, 166 MB of L2 traffic per stage against 117 MB of compulsory
traffic). At 4096² the run-time choice keeps the plain nest (1.39 s, against
1.50 s with the jam).

## Time skewing (`skew`, `gpu_time_skew.cpp`)

After fusion each Runge-Kutta stage is one launch, which at 2048² runs at
the L2 bandwidth. At 4096² the arrays (6 × 67 MB) do not fit in the 128
MiB L2, and every stage streams them all from DRAM. Each point of a stage
only reads points of the previous stage within a few rows of its own, so
the stages can run on a band of rows that fits in the cache, several in a
row, before moving to the next band: the band's data then comes from DRAM
once per few stages instead of once per stage. This is time skewing over
bands of rows; it reuses the kernels as they are and changes only the host
loop and the row range of each launch.

The pass takes a DO loop (or a perfect nest of DO loops with rectangular
bounds, such as `do step` / `do stage`) whose body, run on the host, only
assigns scalars of intrinsic type, tests them (IF, SELECT CASE) and
launches parallel loops assigned to the device; host expressions read no
array (but array bounds and sizes) and call nothing. Such a body can be
replayed: run again from saved copies of the scalars it assigns, it makes
the same decisions and launches the same loops with the same scalar
arguments. With n the number of iterations of the nest, the pass emits

```
do while (t0 < n)                 ! s iterations of the nest at a time
  save the scalars the body assigns
  do b = 0, nb                    ! band by band, in order
    restore them; k = 0
    do t = t0, t0 + s - 1         ! the loop indices from t
      body, where the k-th parallel loop launched runs only its rows
      in [B + b R - k sigma, B + (b + 1) R - 1 - k sigma] (the first band
      reaching down to minus infinity, the last up to infinity), k += 1
  t0 = t0 + s
```

For each launch the windows of the bands partition the rows, so every
iteration of every loop runs exactly once, and the rows of one launch keep
their order. A point of the k-th launch in a replay, at row r, runs in
band floor((r - B + k sigma) / R). If it reads (or overwrites) row r + o of
an array that the k'-th launch, k' < k, writes (or reads), that point of
the earlier launch runs in band floor((r + o - B + k' sigma) / R), which is
not later when o <= (k - k') sigma; o <= sigma suffices. Outputs of two
launches to the same row keep their order for the same reason. The pass
therefore requires:

* every array a launched loop writes (other than a BLOCK's private ones)
  is written at one subscript, the row, equal to the loop's outermost
  index;
* every read of such an array, in any of the loops, has a row subscript at
  most a constant number of rows ahead of the reading loop's outermost
  index, proved with the affine analysis on a copy of the body with its
  pure functions inlined, under the conditions of the branches around the
  read and the bounds of the loop (a boundary formula `q(i, 1)` under
  `j - 1 == 1` is 0 rows ahead). The largest such distance is sigma. A
  lower bound of an array that the subscript of an inlined dummy is shifted
  by is assumed to be 1 and checked by the host before it skews;
* no array is aliased by a pointer, no loop assigns a host scalar (only
  private ones), and a body contains no loop of its own.

The cost model runs on the host before the loop: the bytes of the arrays
that the loops of most iterations touch (the original loops a fusion keeps
for the last iteration are left out), and of one row of each. It skews when
they exceed the cache, with bands of R = (3/4 cache - arrays without rows) /
(bytes per row) rows (at least 8), and s = R / (2 sigma) iterations per
band, between 2 and 32, so that the skew stays small against the band.
The rows start at the first loop's lower bound. Otherwise the original loop
runs.

Index-set splitting then splits the windowed loops by the bounds of their
domain, which the pass records for them (`gpu_window()`): the host computes
the rows of the interior inside the window, the frame rows below and above
it and the side strips of the window's interior rows are packed in front as
before, and the rows the jam leaves over go to the frame. Within a band
that fits in the cache the run-time choice picks the jammed interior.

A prototype first measured the idea on the generated stage kernel, launched
on skewed row ranges with the same checksum as the unskewed order (4096²,
ms per step): 1.330 unskewed; 256 rows × 4 stages 0.704, 256 × 8 0.626,
512 × 8 0.583, 1024 × 16 0.530, 1024 × 32 0.510, 1366 × 16 0.759 (the band
no longer fits), 2048 × 16 1.344. At 2048² and 1024², which fit in the
cache, bands were 5-30% slower, so the cost model keeps those as they are.

Effect (4096², 1000 steps): 1.390 s to 0.556 s, the stage kernels now run
from L2 like those of 2048² (bands of about 1000 rows, 32 stages; sigma
is 5, as the one-sided formulas of the boundary rows reach five rows
ahead). The final field differs from the unskewed build by at most 6.3e-7
(the jammed and plain interiors contract floating-point operations
differently). Every validation case (Formal's strict comparison with the
CPU oracle) passes with `--gpu-cache-bytes` 4 KiB, 64 KiB and 256 KiB,
which between them skew the cases from 13 × 11 to 2048² (bands of 8 rows
and more).

## Results

Formal's 2D advection-diffusion example, `--workspace-kernel split` (the
original source), 1000 steps, `solve_seconds` (medians of 5 interleaved
runs), with the transformations enabled cumulatively. 4096² uses the
analytic initial state (`--cells 4096,4096`, with Formal's 512 MiB
working-set guard raised for the benchmark), the other sizes the shared
inputs. The roof is the compulsory traffic of 16 bytes per cell and step at
5.5 TB/s (the L2-resident copy rate) for 1024² and 2048² and at 1.598 TB/s
(DRAM) for 4096².

| transformations | 1024² | 2048² | 4096² |
|---|---:|---:|---:|
| none | 0.0879 | 0.5435 | 3.066 |
| fuse | 0.1190 | 0.4096 | 2.561 |
| fuse, swap | 0.1048 | 0.3614 | 2.192 |
| fuse, swap, split | 0.0479 | 0.1341 | 1.391 |
| fuse, swap, split, jam | 0.0463 | 0.1186 | 1.390 |
| all (+ skew) | **0.0431** | **0.1186** | **0.556** |
| % of the roof (all) | 7.1% | 10.3% | 30.2% |
| speed-up (all / none) | 2.0x | 4.6x | 5.5x |

The rows up to the jam were measured with the commits that introduced
them; the last row in a later run (none: 0.0846 / 0.5441 / 3.069 s), where
skewing, which the cost model only picks at 4096², leaves 1024² and 2048²
as they were (1024² within noise of 0.0432 s).

`solve_seconds` includes copying the fields to the device at the start of
the solve and back at its end (about 5.5 ms at 1024², 20 ms at 2048² and
95 ms at 4096²); without it the timesteps take 38 ms, 99.5 ms and 0.46 s
(1.30 s before skewing), 8%, 12% and 36% of the roof. Fusion alone is slower at 1024², where the two
original kernels are short, until index-set splitting removes the boundary
branches it duplicates.

For comparison, the hand-written C + CUDA of the same method (built from
Formal's `benchmarks/2d-advection-cuda`, measured in the same runs as the
last row): `split` (the same two kernels per stage) 0.0740 / 0.5547 /
3.023 s; `regyf` (fused stage kernel, register blocking, interior and
boundary kernels) 0.0388 / 0.1102 / 1.151 s; `lean2` 0.0375 / 0.0654 s
(two stages per kernel); `pair` 0.652 s and `march` 0.294 s at 4096² (two
and four stages per kernel). The compiled Fortran is at the level of the
hand-written per-stage kernels at 1024² and 2048², and at 4096² faster
than them and than `pair`, since the bands keep several stages in the L2
cache; what remains is fusing stages within a kernel, which these passes
do not do.

## What was tried and not kept

* The frame as a launch of its own: the frame's iterations are latency
  bound (8 µs per stage at 2048², a few blocks on a mostly idle device).
* Merging the frame into the interior's launch before the offload pipeline
  could tell which index is contiguous: the stores of the frame, at
  computed indices, made it number the threads along the wrong dimension
  (5x slower), hence the rule above.
* Unroll-and-jam along the contiguous dimension (two columns per thread):
  strided accesses and the wrong thread numbering, 4x slower.
* Jam factors of 2 and 4 along the outer dimension (2048²: 0.1353 and
  0.1346 s against 0.1326 s for 3).
* Tiles of 64 or 128 iterations of the contiguous dimension, and 512
  threads per block: within 3% either way.
* Exchanging the ping-pong buffers with `move_alloc`: the host statements it
  lowers to make the offload pipeline wait for the device every stage.
* Unswitching a test the loop does not change (`select case (stage)`, a
  kernel per stage): 5% faster at 2048², 6% slower at 1024², where the
  launches of four different kernels leave longer gaps between them.
* Staging the stencil's input tile and halo in shared memory (block
  cooperative, as in the hand-written `regyf`): a prototype of the jammed
  2048² stage kernel with `q` staged went from 25.9 µs to 32.9 µs, with
  the same L2 traffic and 35% more instructions; the L1 cache already
  serves the halo's reuse. Its L2 traffic above the compulsory one comes
  from rows that do not start on a sector (leading dimension 2050): an
  aligned layout cut L2 sectors by 15% but the time by 4% only.
* Faster copies at the start and end of a solve: a pageable host-to-device
  copy of 16.8 MB takes 1.34-1.44 ms (12 GB/s), pinning the buffer for it
  (`cudaHostRegister`, copy, unregister) 1.7-2.0 ms, and staging through a
  pinned buffer with 1 to 8 threads 4.2-5.7 ms. The copies themselves
  (the fields read, and the field written back) are what the runtime's
  model requires.
* CUDA graphs over the repeated launches: at 1024² the device is idle
  between launches for 0.29 µs (median), 1.2 ms of the 38 ms of time
  steps, so a graph could not save more than 3%.
* Fusing two Runge-Kutta stages into one kernel, measured first on
  prototypes of the interior (the stencil without the boundary rows,
  written in the form a compiler pass would generate; all variants give
  the same checksum as two separate stages). Two per-stage launches of a
  plain one-cell-per-thread kernel take 44.8 µs at 2048² (22.4 µs per
  stage, at the L2 roof of the per-stage scheme; the generated per-stage
  kernel takes 24.7 µs). Fused versions:
  * block-cooperative, overlapped tiling in x (6 halo columns recomputed)
    and the block marching down its rows with the input and the first
    stage in shared-memory row rings, two barriers per step: 67-92 µs for
    tiles of 32-128 columns × 4-16 rows and segments of 16-128 rows;
  * register streaming without shared memory: a warp owns 32 columns (20
    after the halo lanes the two stages consume), marches down its rows
    carrying the last rows of the input and of the first stage and the y
    faces in registers (scalar replacement down the column, every face
    computed once), and exchanges x neighbours with warp shuffles:
    39.3 µs (segments of 32 rows; 43.9 µs at 16, 57.6 µs at 128); two
    columns per lane 42-60 µs, several rows per step (more parallel work
    per thread) 47-66 µs;
  * the same streaming for one stage (scalar replacement down the column
    alone): 54-96 µs for two stages, slower than the plain kernel.

  The best fused form is 12% faster than two plain stages at 2048² and
  slower at 1024² (17.6 µs against 13.1 µs), and at 4096² it halves the
  DRAM traffic (322 µs against 602 µs unskewed) but stays above what time
  skewing gets from the plain stages in L2 (about 230 µs per two stages in
  the full solver). The hand-written `lean2`, which reaches 28 µs per two
  stages at 2048² including the boundary rows, adds what a generic pass
  does not: a padded layout without bounds checks, 64-bit loads of column
  pairs, x faces shared through shared memory, 167 registers, and a
  separate boundary grid launched programmatically. A pass for a new
  block- or warp-cooperative iteration mode was not worth building for 12%.
* Capping the registers of the kernels for occupancy (`-maxrregcount`):
  3% faster at 4096² before time skewing (40 registers; 32 was slower),
  where the plain stage kernel was DRAM bound; with the bands in the L2
  cache it no longer is.

## Next passes

* Stage fusion within a kernel, only together with the layout and code
  choices of the hand-written kernels (see the prototypes above): a
  generic fused kernel gains 12% at most over the plain per-stage kernels
  that time skewing keeps in the cache.
* The generated stage kernel takes 24.7 µs at 2048², a plain interior
  kernel of the same stencil 22.4 µs: about 10% is left in the per-stage
  kernel itself (the frame packed into the launch, index arithmetic).
* Keeping arrays on the device across calls of a solver (the copies at the
  start and end of a solve are 13-17% of `solve_seconds`): the runtime
  would have to know that the host did not write them between the calls.
