# GpuLaneBarrier

The lanes of an iteration wait for each other.

## Declaration

### Syntax

```text
GpuLaneBarrier()
```

### Arguments

None.

### Return values

None.

## Description

In a kernel whose layout has `lanes` (see
[gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)), each iteration
runs on several threads of one SIMD-group, its lanes. **GpuLaneBarrier**
makes every lane of the iteration wait until all of them have reached it,
and makes what each wrote to memory before it visible to the others after
it. Every lane of the iteration reaches the same barriers: one is placed
only where all of them go. With one lane per iteration it does nothing.
Only such a kernel has one.

## Examples

```clojure
(GpuLaneBarrier)
```

## See Also

[GpuLaneIndex](../expression_nodes/GpuLaneIndex.md), [GpuLaneBroadcast](../expression_nodes/GpuLaneBroadcast.md), [GpuLaneBarrier](../statement_nodes/GpuLaneBarrier.md), [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)
