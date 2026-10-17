# GpuLaneIndex

The lane of the running thread within its iteration.

## Declaration

### Syntax

```text
GpuLaneIndex(ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the type of the expression, an integer. |
| `value` | always `nil`: the lane is only known when the kernel runs. |

### Return values

The value of the expression.

## Description

In a kernel whose layout has `lanes` (see
[gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)), each iteration
runs on `lanes` consecutive threads of one SIMD-group. The thread index
([GpuThreadIndex](GpuThreadIndex.md)) numbers the iterations, and
**GpuLaneIndex** tells apart the lanes of one: it counts from zero to
`lanes - 1`. A loop whose iterations the lanes share starts at its first
iteration plus the lane and steps by `lanes` iterations. Only such a kernel
has one.

## Examples

```clojure
(GpuLaneIndex
  :type (Integer
    :kind 4
  )
  :value nil
)
```

## See Also

[GpuLaneIndex](../expression_nodes/GpuLaneIndex.md), [GpuLaneBroadcast](../expression_nodes/GpuLaneBroadcast.md), [GpuLaneBarrier](../statement_nodes/GpuLaneBarrier.md), [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)
