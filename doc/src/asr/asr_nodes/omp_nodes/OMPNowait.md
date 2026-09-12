# OMPNowait

`nowait`: do not wait at the end of the region.

## Declaration

### Syntax

```text
OMPNowait()
```

### Arguments

None.

### Return values

None.

## Description

A worksharing region normally ends with a barrier. `nowait` removes it, so a
thread that finishes its share continues immediately.

## Examples

```clojure
(OMPNowait)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPSchedule](OMPSchedule.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
