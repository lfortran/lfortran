# OMPSchedule

`schedule`: how loop iterations are divided among the threads.

## Declaration

### Syntax

```text
OMPSchedule(schedule_type kind, expr? chunk_size)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | which schedule; see [schedule_type](schedule_type.md). |
| `chunk_size` | how many iterations to hand out at a time, or `nil` for the default. |

### Return values

None.

## Description

`Static` divides the iterations up front, which is cheapest when they cost the
same. `Dynamic` and `Guided` hand out chunks as threads become free, which
costs more but tolerates uneven work. `Auto` leaves the choice to the
implementation and `Runtime` to an environment variable.

## Examples

```clojure
(OMPSchedule
  :kind :Static
  :chunk_size (IntegerConstant
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

[schedule_type](schedule_type.md), [OMPCollapse](OMPCollapse.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
