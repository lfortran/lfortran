# OMPThreadLimit

`thread_limit`: the largest team size allowed.

## Declaration

### Syntax

```text
OMPThreadLimit(expr thread_limit)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `thread_limit` | the maximum number of threads per team. |

### Return values

None.

## Description

It caps how many threads a team of the region may have, which matters on a
device whose resources per team are limited.

## Examples

```clojure
(OMPThreadLimit
  :thread_limit (IntegerConstant
    :n 64
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPNumTeams](OMPNumTeams.md), [OMPNumThreads](OMPNumThreads.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
