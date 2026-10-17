# gpu_kernel_layout

The finalized launch contract owned by a kernel's `Function.gpu` field.

## Declaration

### Syntax

```text
gpu_kernel_layout = (gpu_kernel_argument* buffers, gpu_kernel_argument* scalars,
    gpu_workspace* workspaces, bool packed, int source_argument_count,
    symbol* device_functions, symbol? lanes)
gpu_kernel_argument = (symbol variable, symbol? member, ttype type,
    gpu_argument_kind kind, int argument_index, int dimension, bool written)
gpu_workspace = (symbol variable, gpu_workspace_dimension* dims,
    int buffer_index, int element_size)
gpu_workspace_dimension = (expr extent, symbol? parameter)
```

## Description

`buffers` and `scalars` give the binding order. Each entry names its kernel
dummy by symbol and argument position. A decomposed derived-type component
also names its member symbol. `type` is the explicit scalar or buffer element
type. `dimension` identifies an array dimension or a packed-buffer offset,
according to the entry's role. `written` says whether the kernel may write a
buffer: it is false only when every reference of the kernel to the dummy
only reads it. The runtime keeps a buffer the kernel may write on the
device after the launch, and copies one it only reads when the launch is
made (see [GPU offloading](../../../gpu_offloading.md)).

A workspace names a distinct local array, even when another scope declares
an array with the same name. Its slot follows the argument bindings and its
element size agrees with its element kind.

Each dimension's `extent` is a host-evaluable expression over the original
kernel arguments. A constant has no `parameter`; a runtime extent has a
distinct appended scalar parameter. The host evaluates the expression once
and both allocation and device indexing consume that value.

`source_argument_count` separates original arguments from these appended
parameters. `device_functions` records the device call graph in callee-first
order; direct self-recursion is permitted.

`lanes`, when present, is the kernel's last parameter, an integer scalar
after the extent parameters: each iteration of the kernel runs on that many
consecutive threads of one SIMD-group, its lanes, which share the loops
inside it ([GpuLaneIndex](../expression_nodes/GpuLaneIndex.md),
[GpuLaneBarrier](../statement_nodes/GpuLaneBarrier.md),
[GpuLaneBroadcast](../expression_nodes/GpuLaneBroadcast.md)). The thread
index the kernel reads then numbers its iterations, the host starts
`lanes` threads per iteration, and a workspace slice belongs to an
iteration. `gpu_vector_lanes` sets it, after the finalizer.

The finalizer produces this metadata after shared lowering. Host expansion
and device emission read it without reconstructing the ABI.

See [Function](../symbol_nodes/Function.md),
[GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), and
[gpu_argument_kind](../enum_nodes/gpu_argument_kind.md).
