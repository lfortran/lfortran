# OMPPrivate

`private`: each thread gets its own uninitialised copy.

## Declaration

### Syntax

```text
OMPPrivate(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the variables the clause applies to. |

### Return values

None.

## Description

The copies start undefined, and the original variable is unchanged when the
region ends. [OMPFirstPrivate](OMPFirstPrivate.md) is the variant that copies
the value in, and [OMPLastPrivate](OMPLastPrivate.md) the one that copies it
back out.

## Examples

```clojure
(OMPPrivate
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

[OMPShared](OMPShared.md), [OMPFirstPrivate](OMPFirstPrivate.md), [OMPLastPrivate](OMPLastPrivate.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
