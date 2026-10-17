# GpuThreadIndex

The index of the running thread within its block.

## Declaration

### Syntax

```text
GpuThreadIndex(int dim, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `dim` | which dimension to report, counting from zero. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Every thread of a kernel runs the same body, and this is what tells them
apart. The index counts from zero within the block; the position in the whole
grid is `GpuBlockIndex * GpuBlockSize + GpuThreadIndex`. In a kernel whose
layout has `lanes` (see
[gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)), that position
numbers the iterations, which several threads share: the lanes, which
[GpuLaneIndex](GpuLaneIndex.md) tells apart.

## Examples

```clojure
(GpuThreadIndex
  :dim 0
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_kernel.asr
:language: clojure
```

## See Also

[exec_space](../enum_nodes/exec_space.md), [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), [GpuThreadIndex](GpuThreadIndex.md), [GpuBlockIndex](GpuBlockIndex.md), [GpuBlockSize](GpuBlockSize.md)
