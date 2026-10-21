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
comma separated list of `fuse` and `swap` (`swap` only applies to loops
that are fused); any other value is an error.

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
* The intermediate and renamed arrays are not pointers, nor variables (or
  components of variables) with the `target` or `pointer` attribute, since
  reads through a pointer would not be seen.
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
  allocated once, before them).
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

Effect (2048²): 0.410 s to 0.361 s with fusion alone (the 10 µs copy per
stage goes).
