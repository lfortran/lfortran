# OMPShared

`shared`: every thread uses the same variable.

## Declaration

### Syntax

```text
OMPShared(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the variables the clause applies to. |

### Return values

None.

## Description

The threads see one variable, so any write is visible to all of them and any
race is the program's to avoid.

## Examples

```clojure
(OMPShared
  :vars [
    (Var
      :v (SymbolRef 1 "a")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPPrivate](OMPPrivate.md), [OMPReduction](OMPReduction.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
