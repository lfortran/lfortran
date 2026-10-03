# OMPReduction

`reduction`: private copies combined with an operator at the end.

## Declaration

### Syntax

```text
OMPReduction(reduction_op operator, expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `operator` | the operator; see [reduction_op](../enum_nodes/reduction_op.md). |
| `vars` | the variables the clause applies to. |

### Return values

None.

## Description

Each thread accumulates into its own copy, initialised to the identity of the
operator, and the copies are combined into the original when the region ends.
The operators are associative, so the order the copies are combined in does
not matter.

## Examples

```clojure
(OMPReduction
  :operator :ReduceAdd
  :vars [
    (Var
      :v (SymbolRef 1 "total")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[reduction_op](../enum_nodes/reduction_op.md), [OMPPrivate](OMPPrivate.md), [reduction_expr](../helper_nodes/reduction_expr.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
