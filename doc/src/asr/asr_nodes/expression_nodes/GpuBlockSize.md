# GpuBlockSize

The number of threads in a block.

## Declaration

### Syntax

```text
GpuBlockSize(int dim, ttype type, expr? value)
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

This is the `block_size` the kernel was launched with. It is read at run time
rather than baked in, so one kernel can be launched with different
configurations.

## Examples

```clojure
(GpuBlockSize
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
