# reduction_expr

One reduction of a parallel loop.

## Declaration

### Syntax

```text
reduction_expr = (reduction_op op, expr arg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `op` | the operator values are combined with; see [reduction_op](../enum_nodes/reduction_op.md). |
| `arg` | the variable the result is accumulated into. |

### Return values

None.

## Description

Each iteration accumulates into its own copy of `arg`, and the copies are
combined with `op` when the loop ends. The operators are associative, which is
what makes the order the copies are combined in irrelevant.

## Examples

```clojure
(reduction_expr
  :op :ReduceAdd
  :arg (Var
    :v (SymbolRef 1 "total")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/doconcurrentloop_stmt.asr
:language: clojure
```

## See Also

[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md), [reduction_op](../enum_nodes/reduction_op.md), [OMPReduction](../omp_nodes/OMPReduction.md)
