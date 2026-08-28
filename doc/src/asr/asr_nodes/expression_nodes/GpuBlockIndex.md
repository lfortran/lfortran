# GpuBlockIndex

The index of the running block within the grid.

## Declaration

### Syntax

```text
GpuBlockIndex(int dim, ttype type, expr? value)
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

The block index counts from zero, up to the `grid_size` the kernel was
launched with.

## Examples

```clojure
(GpuBlockIndex
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
