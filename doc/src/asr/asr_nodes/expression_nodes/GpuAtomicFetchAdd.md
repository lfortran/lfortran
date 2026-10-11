# GpuAtomicFetchAdd

Adds to an integer in device memory atomically, and gives its old value.

## Declaration

### Syntax

```text
GpuAtomicFetchAdd(expr target, expr increment, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the `integer(4)` variable or array element to add to; it has to be in device memory. |
| `increment` | the `integer(4)` value to add. |
| `type` | the type of the expression, `integer(4)`. |
| `value` | always `nil`: the expression has no compile time value. |

### Return values

The value `target` held just before the addition.

## Description

The addition and the read of the old value are one indivisible operation
with relaxed memory ordering: when several threads add to the same target,
every thread gets a different old value, and the target ends up holding the
sum of all the increments. It is how device code takes memory from the
device heap (see "Allocating on the device" in the GPU offloading design),
and it is only valid in device code.

## Examples

```clojure
(GpuAtomicFetchAdd
  (ArrayItem
    (Var 3 __gpu_heap_state)
    [(()
    (IntegerConstant 2 (Integer 4) Decimal)
    ())]
    (Integer 4)
    ColMajor
    ()
  )
  (Var 3 n)
  (Integer 4)
  ()
)
```

## See Also

[exec_space](../enum_nodes/exec_space.md), [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), [GpuThreadIndex](GpuThreadIndex.md)
