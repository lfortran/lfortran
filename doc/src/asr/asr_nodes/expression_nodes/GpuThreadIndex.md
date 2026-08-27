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
grid is `GpuBlockIndex * GpuBlockSize + GpuThreadIndex`.

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

```{literalinclude} ../../examples/gpukernelfunction.asr
:language: clojure
```

## See Also

[GpuKernelFunction](../symbol_nodes/GpuKernelFunction.md), [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), [GpuThreadIndex](GpuThreadIndex.md), [GpuBlockIndex](GpuBlockIndex.md), [GpuBlockSize](GpuBlockSize.md)
