# OMPCollapse

`collapse`: how many nested loops to share as one iteration space.

## Declaration

### Syntax

```text
OMPCollapse(expr count)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `count` | how many loops to collapse. |

### Return values

None.

## Description

With `collapse(2)` the two outermost loops of the nest are flattened into a
single iteration space before the iterations are shared out, which gives the
threads more work to divide when the outer loop is short.

## Examples

```clojure
(OMPCollapse
  :count (IntegerConstant
    :n 1
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

[OMPSchedule](OMPSchedule.md), [DoLoop](../statement_nodes/DoLoop.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
