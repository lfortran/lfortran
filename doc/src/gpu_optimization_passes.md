# GPU Loop Optimization Passes

The passes described here transform the parallel loops (`do concurrent`)
that the dispatch has assigned to the device, before they are turned into
kernels (see [GPU offloading](gpu_offloading.md)). They are general loop
transformations of the kind a stencil or polyhedral compiler makes; none of
them knows anything about a particular application. Each proves that what it
does preserves the meaning of the program, and leaves the loops as they are
when it cannot.

They run for `--gpu=cuda` and `--gpu=cuda_cpu` (on by default), right after
the dispatch has assigned the loops (`gpu_loop_fusion`, before
`gpu_offload`), so their output is
ordinary `do concurrent` nests and host code that the rest of the offload
pipeline lowers as it lowers any other; Metal keeps its loops as written.
`--gpu-loop-transforms` selects them: `all` (the default), `none`, or a
comma separated list of `fuse` and `swap`.

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

* The producer assigns each array once, at its own iteration, possibly under
  `if` tests that are affine in its indices; its expressions call only pure
  functions and read neither its own arrays nor what the consumer assigns
  (except arrays that are renamed, below).
* The consumer only reads the intermediate arrays, element by element; pure
  functions it passes them to are inlined first.
* Every element the consumer reads is one the producer writes: on every
  path through the consumer (its loop bounds and the `if` tests around the
  read) the subscripts lie in the producer's loop bounds and satisfy its
  tests. A read is inside the bounds of the array, as in any valid program;
  when coverage depends on how an intermediate array was allocated, the
  fused loop runs only when a run-time check finds the array's bounds within
  what the producer writes.
* An array the consumer writes and the producer reads (the stage update of a
  field whose fluxes are computed from it) would be read and written by
  different iterations of the fused loop. The consumer writes a copy of it
  instead; it may read the element it writes before writing it.
* The intermediate arrays keep the values the producer gave them, which the
  program may read later. When the two loops are the body of `do` loops
  (time steps, stages) that touch those arrays nowhere else, have unit steps
  and are not left early, only the last iteration's values can be read
  later: the original loops run in that iteration and the fused loop in all
  others. Without such loops nothing is fused.

Cost model: an element is computed by every iteration that reads it. The
fusion is made when that extra arithmetic, the producer's expression with
the bodies of the functions it calls times the reads beyond the first, is at
most 64 operations per byte of memory traffic removed (a store and a load
per element), the order of a GPU's ratio of arithmetic to memory
throughput.

Effect (2048², 1000 steps): 0.544 s to 0.454 s. The fused stage kernel takes
95 µs, the copy of the renamed field 10 µs, against 137 µs for the two
original kernels.

## Ping-pong buffers (`swap`, part of `gpu_loop_fusion.cpp`)

The copy of a renamed array is copied back after every fused loop unless it
can take turns with the array: the fused loop then reads one of the two and
writes the other, which one alternating from one execution to the next (a
host flag picks between two copies of the fused loop), and nothing is copied.
This is valid when the copy starts equal to the array (a device loop copies
it once, before the `do` loops the fusion relies on) and the two keep
agreeing on every element the consumer does not write: no other statement in
those loops writes the array, the consumer writes the same elements in every
execution (its bounds do not change), and the array is not a target. The
original loops, run in the last iteration, first copy back what the latest
execution left in the copy, if it is there. Exchanging the allocations with
`move_alloc` would do the same, but the host statements around a move make
the offload pipeline wait for the device.

Effect (2048²): 0.410 s to 0.361 s with fusion alone (the 10 µs copy per
stage goes).
