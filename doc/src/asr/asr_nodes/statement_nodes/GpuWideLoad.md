# GpuWideLoad

Reads consecutive elements of an array as one vector.

## Declaration

### Syntax

```text
GpuWideLoad(expr* targets, expr source, expr array, expr? aligned)
```

### Arguments

| Argument | Description |
|---|---|
| `targets` | Variables of the type of the elements; there are as many as fill 16 bytes (2, 4 or 8) |
| `source` | The first element read, an element of `array` |
| `array` | The array `source` is an element of, one the kernel is given |
| `aligned` | A condition on values the launch fixes (the extents and lower bound of `array`) that holds when the elements start at a multiple of 16 bytes if `array` does; absent when they always do |

### Return values

None.

## Description

Assigns `targets(j)` the element that follows `source` by `j - 1` places in
the first dimension of `array`. When `aligned` holds and the data of `array`
starts at a multiple of 16 bytes, the device reads the 16 bytes at `source` at
once; otherwise it reads the elements one by one. Either way each target holds
what the element holds, and every element is one that the statement after
the node reads, so none is read that the program does not read. The node
is made by the `gpu_wide_loads` pass in device code on an NVIDIA GPU and
is only allowed there.

## Examples

```clojure
(GpuWideLoad
    [(Var 3 __gpu_wide) (Var 3 __gpu_wide1) (Var 3 __gpu_wide2) (Var 3 __gpu_wide3)]
    (ArrayItem (Var 3 w) [(() (IntegerBinOp (IntegerBinOp (IntegerConstant 4 (Integer 4)) Mul (Var 3 c) (Integer 4) ()) Sub (IntegerConstant 3 (Integer 4)) (Integer 4) ()) ()) (() (Var 3 n) ())] (Integer 4) ColMajor ())
    (Var 3 w)
    ())
```

## See Also

[GpuLaneBarrier](GpuLaneBarrier.md), [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md)
