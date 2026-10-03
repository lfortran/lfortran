# OMPFirstPrivate

`firstprivate`: a private copy initialised from the original.

## Declaration

### Syntax

```text
OMPFirstPrivate(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the variables the clause applies to. |

### Return values

None.

## Description

Like [OMPPrivate](OMPPrivate.md), except that each copy starts with the value
the variable had when the region was entered.

## Examples

```clojure
(OMPFirstPrivate
  :vars [
    (Var
      :v (SymbolRef 1 "n")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPPrivate](OMPPrivate.md), [OMPLastPrivate](OMPLastPrivate.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
