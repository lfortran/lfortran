# reduction_op

The operators a parallel reduction can combine values with.

## Declaration

### Syntax

```text
reduction_op
    = ReduceAdd
    | ReduceSub
    | ReduceMul
    | ReduceMIN
    | ReduceMAX
    | ReduceIAND
    | ReduceIOR
    | ReduceIEOR
```

### Values

| Value | Meaning |
|----------|-------------|
| `ReduceAdd` | sum. |
| `ReduceSub` | difference. |
| `ReduceMul` | product. |
| `ReduceMIN` | minimum. |
| `ReduceMAX` | maximum. |
| `ReduceIAND` | bitwise and. |
| `ReduceIOR` | bitwise or. |
| `ReduceIEOR` | bitwise exclusive or. |

### Return values

None. An enumeration value is not evaluated.

## Description

Used by `do concurrent` locality specifiers and by the OpenMP `reduction`
clause. Every operator here is associative, which is what lets the iterations
be combined in any order.

## See Also

[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md), [reduction_expr](../helper_nodes/reduction_expr.md), [OMPReduction](../omp_nodes/OMPReduction.md)
