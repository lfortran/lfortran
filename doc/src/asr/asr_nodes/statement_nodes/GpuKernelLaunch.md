# GpuKernelLaunch

Launches a GPU kernel.

## Declaration

### Syntax

```text
GpuKernelLaunch(symbol kernel, expr grid_size, expr block_size,
    call_arg* args)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kernel` | the [Function](../symbol_nodes/Function.md) to run. Its signature has `exec_space = Kernel`; see [exec_space](../enum_nodes/exec_space.md). |
| `grid_size` | the number of blocks to launch. |
| `block_size` | the number of threads per block. |
| `args` | the arguments passed to every thread. |

### Return values

None.

## Description

A launch is not a call: it starts `grid_size * block_size` threads and, unless
the backend says otherwise, returns without waiting for them.
[GpuSync](GpuSync.md) is what waits.

The execution configuration is part of the node rather than of the kernel,
because the same kernel is normally launched with different sizes.

The kernel is an ordinary [Function](../symbol_nodes/Function.md); what makes
it launchable is that its signature has `exec_space = Kernel`, and a kernel
has no result. See [exec_space](../enum_nodes/exec_space.md).

During extraction a launch belongs to a [GpuOffload](GpuOffload.md) candidate
alongside the original CPU alternative. `gpu_kernel_finalize` makes the
definitive decision after shared lowering and records the accepted kernel's
`Function.gpu` layout. `device_launch_expand` and both device emitters consume
that layout. Workspace sizes are evaluated once on the host and passed as
explicit scalar arguments, so host allocation and device indexing use the
same values.

Before creating a launch, GPU offload checks the allocation of allocatable
array results in callees that remain out of line. Nested or multiple allocation
sites must agree with a buffer shape established by an unconditional fixed
allocation. Otherwise the loop is rejected while the original loop is still
available; `--gpu-allow-cpu-fallback` instead runs that loop on the CPU.
Reallocation and conditional assignments remain eligible when they preserve
every extent, not just the total element count.

Every declined offload is a compile-time error unless
`--gpu-allow-cpu-fallback` is explicitly enabled, including native backend
limitations. With that flag, a decline warns and selects the CPU alternative.
`--gpu-decline-stats` reports the `not-implemented` or `backend-cannot`
category without changing this policy. A manually constructed launch without
a CPU alternative cannot use fallback even with the flag.

## Examples

```clojure
(GpuKernelLaunch
  :kernel (SymbolRef 1 "zero")
  :grid_size (IntegerConstant
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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[Function](../symbol_nodes/Function.md), [exec_space](../enum_nodes/exec_space.md), [GpuSync](GpuSync.md), [GpuThreadIndex](../expression_nodes/GpuThreadIndex.md)
