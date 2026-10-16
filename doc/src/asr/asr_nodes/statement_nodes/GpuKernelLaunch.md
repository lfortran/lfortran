# GpuKernelLaunch

Launches a GPU kernel.

## Declaration

### Syntax

```text
GpuKernelLaunch(symbol kernel, expr thread_count, expr block_size,
    call_arg* args, symbol? component_fit, gpu_scratch_buffer* scratch,
    expr? stop_status, stmt* stop_replay)
gpu_scratch_buffer = (int argument_index, expr size, expr? read_back)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kernel` | the [Function](../symbol_nodes/Function.md) to run. Its signature has `exec_space = Kernel`; see [exec_space](../enum_nodes/exec_space.md). |
| `thread_count` | the number of threads that run the kernel's body, one per iteration of the loop; a count below one runs none. |
| `block_size` | the number of threads per block. |
| `args` | the arguments passed to every thread. |
| `component_fit` | the [Block](../symbol_nodes/Block.md) the host runs before the launch to give the allocatable array components of elements of an array argument the loop writes the storage the kernel writes into, or `nil` when the loop writes none. |
| `scratch` | the arguments that are buffers only the device holds (below). |
| `stop_status` | the host array passed as the status buffer of a kernel that can `stop`, or `nil`. |
| `stop_replay` | the host statements that run the `stop` a thread of the launch recorded (below). |

### Return values

None.

## Description

A launch is not a call: it starts `thread_count` threads, rounded up to whole
blocks of `block_size`, and returns without waiting for them. The threads are
numbered from zero, block after block; the kernel sends every thread numbered
`thread_count` or higher back before its body, so the host sizes what the
launch holds per thread -- a workspace slice -- by `thread_count`, not by the
threads it starts. [GpuSync](GpuSync.md) is what waits; the `gpu_launch_sync`
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

Private array storage is placed after shared lowering by `gpu_kernel_storage`,
before address-space specialization. Fixed-size arrays that fit the device's
stack budget have `Thread` memory space. Larger arrays have `Global` memory
space and the kernel layout records their per-thread workspace slices. This
placement never turns an iteration-private value into a shared value.

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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[Function](../symbol_nodes/Function.md), [exec_space](../enum_nodes/exec_space.md), [GpuSync](GpuSync.md), [GpuDiscard](GpuDiscard.md), [GpuThreadIndex](../expression_nodes/GpuThreadIndex.md)
