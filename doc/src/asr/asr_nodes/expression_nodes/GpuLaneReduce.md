# GpuLaneReduce

What the lanes of the iteration hold, combined.

## Declaration

### Syntax

```text
GpuLaneReduce(expr arg, reduction_op op, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | an integer or real scalar. |
| `op` | how the values combine: `ReduceAdd`, `ReduceMul`, `ReduceMIN` or `ReduceMAX`. |
| `type` | the type of `arg`. |
| `value` | always `nil`. |

### Return values

The values `arg` has in the lanes of the running thread's iteration,
combined by `op`, in every lane.

## Description

In a kernel whose layout has `lanes` (see
[gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)), the lanes of an
iteration may share out a loop that accumulates into private scalars -- the
inner product of a matrix-vector product, the forces on one body -- each
accumulating its iterations from the identity of the operation (the first
lane from the value the scalar had before the loop). After the loop every
lane takes what all of them hold: `s = GpuLaneReduce(s, ReduceAdd)`. Every
lane of the iteration evaluates it at the same point. Only a kernel for a
device whose lanes can combine values has one
(`GpuDeviceCapabilities::lane_reductions`, see `gpu_vector_lanes.cpp`).

## Examples

```clojure
(GpuLaneReduce
  (Var 2 t)
  ReduceAdd
  (Real
    :kind 8
  )
  ()
)
```

## See Also

[GpuLaneIndex](../expression_nodes/GpuLaneIndex.md), [GpuLaneBroadcast](../expression_nodes/GpuLaneBroadcast.md), [GpuLaneBarrier](../statement_nodes/GpuLaneBarrier.md), [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)
