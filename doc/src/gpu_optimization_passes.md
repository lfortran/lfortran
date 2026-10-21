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
runs `gpu_loop_fusion`, then `gpu_index_set_split`, before `gpu_offload`;
each of the two can also be run on its own with `--pass`), so their output is
ordinary `do concurrent` nests and host code that the rest of the offload
pipeline lowers as it lowers any other; Metal keeps its loops as written.
`--gpu-loop-transforms` selects them: `all` (the default), `none`, or a
comma separated list of `fuse`, `swap`, `split` and `jam` (`swap` only
applies to loops that are fused); any other value is an error. The
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
A parallel loop in a procedure that a device loop calls runs on one thread
of the kernel, one iteration after the other, so such procedures are not
split.

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
