# OMPNumThreads

`num_threads`: how many threads to use.

## Declaration

### Syntax

```text
OMPNumThreads(expr num_threads)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `num_threads` | the number of threads requested. |

### Return values

None.

## Description

It requests a team size for this region only. The implementation may give
fewer threads.

## Examples

```clojure
(OMPNumThreads
  :num_threads (IntegerConstant
    :n 4
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

[OMPIf](OMPIf.md), [OMPThreadLimit](OMPThreadLimit.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
