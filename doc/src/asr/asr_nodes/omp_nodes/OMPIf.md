# OMPIf

`if`: run the region in parallel only when a condition holds.

## Declaration

### Syntax

```text
OMPIf(expr condition)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `condition` | the condition, of a logical type. |

### Return values

None.

## Description

When the condition is false the region runs as if it had not been marked
parallel at all, which is how a loop too small to be worth parallelising
avoids the cost of starting threads.

## Examples

```clojure
(OMPIf
  :condition (Var
    :v (SymbolRef 1 "run")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPNumThreads](OMPNumThreads.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
