# OMPLastPrivate

`lastprivate`: a private copy whose final value is copied back.

## Declaration

### Syntax

```text
OMPLastPrivate(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the variables the clause applies to. |

### Return values

None.

## Description

The copy belonging to the sequentially last iteration is written back to the
original when the region ends, so the variable holds what a serial run would
have left in it.

## Examples

```clojure
(OMPLastPrivate
  :vars [
    (Var
      :v (SymbolRef 1 "i")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPPrivate](OMPPrivate.md), [OMPFirstPrivate](OMPFirstPrivate.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
