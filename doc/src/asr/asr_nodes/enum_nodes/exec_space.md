# exec_space

Where a procedure runs.

## Declaration

### Syntax

```text
exec_space = Host | Device | HostDevice | Kernel
```

### Values

| Value | Meaning |
|----------|-------------|
| `Host` | ordinary CPU code. This is what every procedure is until something says otherwise. |
| `Device` | runs on the GPU, and is called from other device code. |
| `HostDevice` | reachable from the host and from the device alike, and emitted for both. |
| `Kernel` | the entry point of a GPU kernel: the host launches it with a [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md). |

### Return values

None. An enumeration value is not evaluated.

## Description

The execution space is a member of
[FunctionType](../type_nodes/FunctionType.md), so it belongs to the signature
of the procedure rather than to its symbol.

`Device` and `Kernel` answer different questions. Both run on the GPU, but a
`Device` procedure is an ordinary callee, while a `Kernel` is what a launch
names: it takes the `__global__` qualifier of CUDA or the `kernel` qualifier
of Metal, its arguments are bound as buffers, and it returns nothing.

Only the kernels are marked by the frontend. The `device_partition` pass takes
the closure of the call graph from them and gives every procedure it reaches
the space it belongs in, so a procedure called from both a kernel and the host
becomes `HostDevice`.

## See Also

[FunctionType](../type_nodes/FunctionType.md), [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), [memory_space](memory_space.md), [Function](../symbol_nodes/Function.md)
