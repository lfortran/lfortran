# GpuKernelLaunch

Launches a GPU kernel.

## Declaration

### Syntax

```text
GpuKernelLaunch(symbol kernel, expr thread_count, expr block_size,
    call_arg* args, symbol? component_fit, gpu_scratch_buffer* scratch,
    expr? stop_status, stmt* stop_replay, expr* overwritten,
    expr* overwritten_if, gpu_launch_dimension* chunk_dimensions,
    int chunk_fixed_iterations, symbol? first_chunk_parameter)
gpu_scratch_buffer = (int argument_index, expr size, expr? read_back)
gpu_launch_dimension = (symbol start_parameter, symbol end_parameter)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kernel` | the [Function](../symbol_nodes/Function.md) to run. Its signature has `exec_space = Kernel`; see [exec_space](../enum_nodes/exec_space.md). |
| `thread_count` | the number of threads the launch numbers: one per iteration of the loop, or, when the nest is launched in tiles, one per place in whole tiles; a count below one runs none. |
| `block_size` | the number of threads per block. |
| `args` | the arguments passed to every thread. |
| `component_fit` | the [Block](../symbol_nodes/Block.md) the host runs before the launch to give the allocatable array components of elements of an array argument the loop writes the storage the kernel writes into, or `nil` when the loop writes none. |
| `scratch` | the arguments that are buffers only the device holds (below). |
| `stop_status` | the host array passed as the status buffer of a kernel that can `stop`, or `nil`. |
| `stop_replay` | the host statements that run the `stop` a thread of the launch recorded (below). |
| `overwritten` | array arguments the kernel may assign whole before it reads them (below). |
| `overwritten_if` | for each of `overwritten`, the host condition under which it does. |
| `chunk_dimensions` | the dimensions of the iteration space a launch of more iterations than the chunk limit splits into chunks, each run as a launch of the kernel, in the order the kernel numbers its iterations (below); empty when it always runs as one. |
| `chunk_fixed_iterations` | the product of the constant extents of the dimensions every chunk covers whole: 1 when there are none, and 0 when `chunk_dimensions` is empty. |
| `first_chunk_parameter` | the kernel parameter a launch with `chunk_dimensions` sets to 1 when it runs as one or for its first chunk, and to 0 for the later chunks, whatever it passes for it in `args`, so that only the threads of the first chunk start the accumulators of its reductions (below), or `nil`. It is not a bound parameter of `chunk_dimensions`. |

### Return values

None.

## Description

A launch is not a call: it starts `thread_count` threads, rounded up to whole
blocks of `block_size`, and returns without waiting for them. The threads are
numbered from zero, block after block; the kernel sends every thread numbered
`thread_count` or higher back before its body, so the host sizes what the
launch holds per thread -- a workspace slice -- by `thread_count`, not by the
threads it starts.

On a device that asks for tiles (CUDA), a nest of two or more dimensions
without a reduction can be launched in tiles: a block covers a tile of 32
iterations of the dimension its stores are contiguous in by the rest of the
block along a second dimension, the one the stores step through next in
memory (or, if they do not agree on one, the next one in loop order). The
block index numbers the tiles, and `thread_count` counts whole tiles. Where
whole tiles would pad the nest by more than a quarter, as a short dimension
does, or would start more threads than the chunk limit (below), the host
tells the kernel to number the iterations flat instead, and `thread_count`
counts iterations. Padding to whole tiles can take
`thread_count` past the kind of the loop indices, so it is an `integer(8)`.
The kernel checks the zero-based tile coordinates against the extents before
adding the loop lower bounds. A padded thread returns before constructing a
source index that could overflow even when every valid index is representable.
[GpuSync](GpuSync.md) is what waits; the `gpu_launch_sync`
pass places it where the host next needs what a kernel wrote (see
[GPU offloading](../../../gpu_offloading.md)).

The execution configuration is part of the node rather than of the kernel,
because the same kernel is normally launched with different sizes.

The kernel is an ordinary [Function](../symbol_nodes/Function.md); what makes
it launchable is that its signature has `exec_space = Kernel`, and a kernel
has no result. See [exec_space](../enum_nodes/exec_space.md).

The `gpu_offload` pass replaces a parallel loop assigned to the device with its
launch, together with the copies the host makes before and after it; nothing
of the original loop is kept. `gpu_kernel_finalize` records the kernel's
`Function.gpu` layout after shared lowering. `device_launch_expand` and both
device emitters consume that layout. Workspace sizes are evaluated once on the host and passed as
explicit scalar arguments, so host allocation and device indexing use the
same values.

Before creating a launch, GPU offload checks the allocation of allocatable
array results in callees that remain out of line. Nested or multiple allocation
sites must agree with a buffer shape established by an unconditional fixed
allocation. Otherwise the loop is rejected while the original loop is still
available, which is an error: it is a lowering LFortran does not have yet.
Reallocation and conditional assignments remain eligible when they preserve
every extent, not just the total element count.

A loop reaches GPU offload only after the unsupported-construct check in the
`parallel_dispatch` pass has assigned it to the device (see
[GPU offloading](../../../gpu_offloading.md)), so a loop the offload cannot
lower is a compile-time error, whatever its reason, whether or not
`--gpu-allow-cpu-fallback` is given, and also with `--show-gpu-kernel-source`.

A `scratch` entry makes the argument `argument_index` a buffer of `size`
elements of the kernel argument's element type that only the device holds,
such as an arena of the device heap: the host allocates nothing for it and
copies nothing in or out. The actual argument is an allocatable array of the
launching procedure, not allocated before the launch. When `read_back` is
present, the host allocates the actual with `read_back` elements after the
launch, at most `size`, and copies the leading elements of the buffer into
it; otherwise the actual stays unallocated.

A kernel that can `stop` records the statement a thread reached in the
status buffer `stop_status`. The statements of `stop_replay` read it and run
that statement on the host. They run after the first GpuSync that follows
the launch, which the `gpu_launch_sync` pass places, copies of them after
each GpuSync the launch may still be pending at; that pass also clears the
first element of `stop_status` once, on entry to the procedure.

When `overwritten_if[i]`, a scalar logical the host evaluates at the launch,
is true, the kernel assigns every element of the array `overwritten[i]`, an
actual argument of the launch, before it reads any of them. What the host
holds there then does not reach the kernel, so `device_launch_expand` hands
the buffer to the runtime as `LFORTRAN_GPU_WRITE_DISCARD`, and a runtime
with memory of its own only makes room for it instead of copying it in.
`gpu_offload` finds such arrays in the loop as the source wrote it: an
assignment `a(i, j, ...) = value` subscripted by the loop variables, one per
loop, that each iteration reaches before anything else of it names `a`, at
the top of the iteration or inside IFs that compare loop variables with
values the loop cannot change. The condition is that those loops, narrowed
by the IFs, run over exactly the bounds of `a`.

This proof does not imply that the buffer is unaliased. If any other buffer
argument of the launch overlaps it, the runtime treats the discard as an
ordinary write so that the other argument can still read its previous
contents. The kernel layout's `gpu_kernel_argument.noalias` is a separate
shared-ASR proof; it is not inferred from `overwritten`.

A launch with `chunk_dimensions` runs as several launches of its kernel when
it has more iterations than the chunk limit (`huge(0)`, which the hidden,
test-only option `--gpu-chunk-limit` lowers): one per chunk, a box of the
iteration space of at most that many iterations. The iterations of a `do
concurrent` may run in any order, so the chunks run one after another, and
the kernel numbers the iterations of each chunk from the bounds of its box,
from zero. A chunk has at most `huge(0)` iterations, so a kernel whose loop
indices are all `integer(4)` numbers them in `integer(4)`, while the launch
counts the iterations of the whole nest in `integer(8)`; an `integer(8)`
index keeps `integer(8)` numbering.
A chunk covers every dimension not in `chunk_dimensions` whole; their
extents are constants, and their product is `chunk_fixed_iterations`, at
most the limit. A bound whose value is known at compile time (a literal, a
named constant or a constant expression) stays a constant in the kernel
unless its dimension is in `chunk_dimensions`. Each dimension in
`chunk_dimensions` names the kernel parameters that hold its bounds,
`start_parameter` and `end_parameter`, which each chunk sets to the bounds
of its box, and the launch reads the bounds of the whole dimension from its
arguments for them. They are listed fastest-varying first, and a chunk
covers as many values of each as fit beside the dimensions covered whole
and the ones before it in the list, and at least one. `gpu_offload` splits
every dimension whose extent is not a constant, and those with constant
extents beyond what fits the limit. It gives a launch no
`chunk_dimensions` when its nest cannot have more iterations than the
limit, as the kind of each loop index and its constant bounds bound its
extent: a one-dimensional `do concurrent (i = 1:n)` with an `integer(4)`
index has at most `huge(0)` iterations. `device_launch_expand` sets the
buffers, scratch buffers and workspaces once, sized for the largest chunk,
and then launches each chunk with its own scalar arguments and grid. A
launch that may run in tiles runs flat when its whole tiles would start
more threads than the chunk limit, with or without `chunk_dimensions`, so no
launch starts more threads than the limit. Where every extent of the nest is
a constant, that is decided
at compile time: a nest whose whole tiles come to more threads than
the limit gets a kernel that numbers its iterations flat, so a nest of at
most that many iterations runs as one launch with no `chunk_dimensions`,
and its constant bounds stay constants in the kernel. A buffer handed over as
`LFORTRAN_GPU_WRITE_DISCARD` is handed over again as written after each
chunk, so the chunks after the first keep what it wrote. A launch whose
kernel runs several threads per iteration (the layout's `lanes`) starts
that many per iteration of each chunk. CUDA and Metal launches have
`chunk_dimensions`. The host counts the iterations of a launch in
`integer(8)`, so a Metal kernel, which numbers them in 32 bits, runs a nest
of more than `huge(0)` iterations in chunks.

A launch with a reduction gives each thread an accumulator of its own, an
element of a buffer argument, which the kernel sets to the identity of the
operator before the body combines into it; the host folds the accumulators
into the scalar after the launch. The threads of each chunk are numbered
from zero, so a launch that may run in chunks has one accumulator per
iteration of its largest chunk, the first one, when it has more iterations
than the chunk limit. Its kernel takes `first_chunk_parameter`, which the
launch sets to 1 when it runs as one or for its first chunk, and to 0 for
the later chunks: the threads of the first chunk start their
accumulators, and those of the later chunks keep combining into what the
thread of the same number left there. A thread past the end of a partial
chunk returns before it touches its accumulator, and the host folds only
the ones the first chunk started, once, after the last chunk. A launch
without `chunk_dimensions`, or whose kernel has no accumulators, has no
`first_chunk_parameter`.

Private array storage is placed after shared lowering by `gpu_kernel_storage`,
before address-space specialization. Fixed-size arrays that fit the device's
stack budget have `Thread` memory space. Larger arrays have `Global` memory
space and the kernel layout records their per-thread workspace slices. This
placement never turns an iteration-private value into a shared value.

A workspace may record `allocation_if`, its captured `allocation_parameter`,
and `allocation_on_true`. Shared kernel planning accepts only stable, total
scalar predicates that are host-evaluable at every launch and dominate every
data use with the recorded polarity. The host captures the predicate once at
the launch and the device uses the same value. When inactive, launch expansion
requests zero scratch bytes without suppressing unrelated kernel work. Array
shape and bounds are unchanged; inactive or empty slices do not form pointers
into absent storage. Thread-varying tests, optional or aliased scalars,
descriptor reads, calls, and other non-total expressions conservatively retain
normal allocation unless another safe enclosing guard proves it unnecessary.

## Examples

```clojure
(GpuKernelLaunch
  :kernel (SymbolRef 1 "zero")
  :thread_count (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :block_size (IntegerConstant
    :n 256
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :args [
    (call_arg
      :value (Var
        :v (SymbolRef 2 "a")
      )
    )
  ]
  :component_fit nil
  :scratch []
  :stop_status nil
  :stop_replay []
  :overwritten []
  :overwritten_if []
  :chunk_dimensions []
  :chunk_fixed_iterations 0
  :first_chunk_parameter nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[Function](../symbol_nodes/Function.md), [exec_space](../enum_nodes/exec_space.md), [GpuSync](GpuSync.md), [GpuDiscard](GpuDiscard.md), [GpuThreadIndex](../expression_nodes/GpuThreadIndex.md)
