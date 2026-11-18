# GpuLaneReduce

What the lanes of the iteration, or of a group of them, hold, combined.

## Declaration

### Syntax

```text
GpuLaneReduce(expr arg, reduction_op op, int width, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | an integer or real scalar. |
| `op` | how the values combine: `ReduceAdd`, `ReduceMul`, `ReduceMIN` or `ReduceMAX`. |
| `width` | zero for all the lanes of the iteration; otherwise a power of two up to 32, the lanes of each group of that many consecutive lanes of a warp. |
| `type` | the type of `arg`. |
| `value` | always `nil`. |

### Return values

The values `arg` has in the lanes of the running thread's iteration (or,
with a `width`, of its group), combined by `op`, in every lane.

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

In a kernel whose iteration has the lanes of a whole block, a short loop
of the iteration may run each of its iterations on a group of consecutive
lanes of a warp, which share out the loops of its body; each group then
combines what its lanes accumulated with a `width` of the group's size
(the scores of the positions of a chunk of an attention, a warp each).
All the lanes of the warp evaluate it.

## Examples

```clojure
(GpuLaneReduce
  (Var 2 t)
  ReduceAdd
  0
  (Real
    :kind 8
  )
  ()
)
```

## See Also

[GpuLaneIndex](../expression_nodes/GpuLaneIndex.md), [GpuLaneBroadcast](../expression_nodes/GpuLaneBroadcast.md), [GpuLaneBarrier](../statement_nodes/GpuLaneBarrier.md), [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)
