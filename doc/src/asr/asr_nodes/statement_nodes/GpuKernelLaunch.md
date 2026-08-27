# GpuKernelLaunch

Launches a GPU kernel.

## Declaration

### Syntax

```text
GpuKernelLaunch(symbol kernel, expr grid_size, expr block_size,
    call_arg* args)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kernel` | the [GpuKernelFunction](../symbol_nodes/GpuKernelFunction.md) to run. |
| `grid_size` | the number of blocks to launch. |
| `block_size` | the number of threads per block. |
| `args` | the arguments passed to every thread. |

### Return values

None.

## Description

A launch is not a call: it starts `grid_size * block_size` threads and, unless
the backend says otherwise, returns without waiting for them.
[GpuSync](GpuSync.md) is what waits.

The execution configuration is part of the node rather than of the kernel,
because the same kernel is normally launched with different sizes.

## Examples

```clojure
(GpuKernelLaunch
  :kernel (SymbolRef 1 "zero")
  :grid_size (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :block_size (IntegerConstant
    :n 256
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :args [
    (call_arg
      :value (Var
        :v (SymbolRef 2 "a")
      )
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[GpuKernelFunction](../symbol_nodes/GpuKernelFunction.md), [GpuSync](GpuSync.md), [GpuThreadIndex](../expression_nodes/GpuThreadIndex.md)
