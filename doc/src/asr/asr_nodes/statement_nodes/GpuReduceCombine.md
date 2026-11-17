# GpuReduceCombine

Combines a reduction of elements of arrays across the threads of a launch,
on the device.

## Declaration

### Syntax

```text
GpuReduceCombine(gpu_reduce_item* items, expr threads, expr scratch,
    expr? first)
gpu_reduce_item = (expr target, expr value, reduction_op op, expr? initial)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `items` | the elements combined into (below). |
| `threads` | the threads that contribute: those numbered below it, as the kernel numbers its threads. |
| `scratch` | an `integer(8)` array argument of the kernel that the launch holds as a scratch buffer of at least `1 + size(items) * threads` elements (see [GpuKernelLaunch](GpuKernelLaunch.md)). |
| `first` | for a launch that may run in chunks and has an item with an `initial` value, the kernel parameter that is not zero in the first chunk, or `nil`. |

An item has these members:

| Member | Description |
|--------|-------------|
| `target` | an element of an array argument of the kernel, an integer or a real of kind 4 or 8. |
| `value` | what each thread contributes to it, of its type. |
| `op` | how the contributions combine: `ReduceAdd`, `ReduceMul`, `ReduceMIN` or `ReduceMAX` (see [reduction_op](../enum_nodes/reduction_op.md)). |
| `initial` | a constant the combination starts from instead of the target, or `nil`. |

### Return values

None.

## Description

A `do concurrent` with `reduce(op: a)` of an array `a` whose iterations name
the same elements of it -- `a(1)`, or `a(k)` for a `k` the loop does not
change -- becomes a kernel in which every thread accumulates its iterations
into a private scalar per element, in a grid-stride loop, and ends with a
**GpuReduceCombine** of those scalars. Every thread of the launch reaches it
(none has returned). After it, each `target` holds `target op c`, where `c`
is the combination of the `value` of every thread numbered below `threads`
(of an iteration that runs on several lanes, whose lanes hold the same
values, the first lane's). The combination does not depend on how the threads
were scheduled: the same launch gives the same bits every time. An item with
an `initial` value holds `initial op c` instead, in the first chunk of a
launch that runs in chunks (`first` not zero) or in the only one (`first` is
`nil`); the later chunks combine into what the earlier ones left. The host
neither allocates nor reads anything for it, so it does not wait for the
launch.

The device code generator lowers it (`asr_to_gpu_c.h`): on an NVIDIA GPU the
threads of each block combine their values, the block writes the result to
its part of `scratch` and arrives at a counter held in its first element, and
the block that arrives last combines the parts of all the blocks in their
order and applies them to the targets, leaving the counter zero for the next
launch of the kernel. The CPU emulation makes each thread a part. Only device
code has one, and only for CUDA.

## Examples

```fortran
ss(1) = 0
do concurrent (i = 1:n) reduce(+: ss)
    ss(1) = ss(1) + x(i)*x(i)
end do
```

becomes a kernel that ends with a **GpuReduceCombine** of one item, whose
`target` is `ss(1)`, whose `value` is the thread's accumulator, whose `op` is
`ReduceAdd` and whose `initial` is `0.0`: the statement before the loop is
not run by the host.

## See Also

[GpuKernelLaunch](GpuKernelLaunch.md), [GpuLaneReduce](../expression_nodes/GpuLaneReduce.md), [reduction_op](../enum_nodes/reduction_op.md)
