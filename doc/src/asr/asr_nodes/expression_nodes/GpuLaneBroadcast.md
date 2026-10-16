# GpuLaneBroadcast

The value an expression has in the first lane of the iteration.

## Declaration

### Syntax

```text
GpuLaneBroadcast(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | an integer, real or logical scalar. |
| `type` | the type of `arg`. |
| `value` | always `nil`. |

### Return values

The value `arg` has in lane zero of the running thread's iteration.

## Description

In a kernel whose layout has `lanes` (see
[gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)), a statement
that must not be repeated by every lane of an iteration -- it writes memory
the lanes share, or takes elements of the device heap -- runs on the first
lane only. Each lane holds its own copy of a scalar local, so after the
statement every lane takes the scalars it assigned from the first one:
`x = GpuLaneBroadcast(x)`. Every lane of the iteration evaluates it at the
same point. Only such a kernel has one.

## Examples

```clojure
(GpuLaneBroadcast
  (Var 2 x)
  (Integer
    :kind 4
  )
  ()
)
```

## See Also

[GpuLaneIndex](../expression_nodes/GpuLaneIndex.md), [GpuLaneBroadcast](../expression_nodes/GpuLaneBroadcast.md), [GpuLaneBarrier](../statement_nodes/GpuLaneBarrier.md), [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)
