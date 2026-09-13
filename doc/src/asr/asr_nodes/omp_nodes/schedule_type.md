# schedule_type

The ways loop iterations can be divided among threads.

## Declaration

### Syntax

```text
schedule_type
    = Static | Dynamic | Guided | Auto | Runtime
```

### Values

| Value | Meaning |
|----------|-------------|
| `Static` | divide the iterations into chunks up front and assign them in order. |
| `Dynamic` | hand out a chunk whenever a thread becomes free. |
| `Guided` | like `Dynamic`, but with chunks that get smaller as the work runs out. |
| `Auto` | let the implementation choose. |
| `Runtime` | take the schedule from the environment at run time. |

### Return values

None.

## Description

`Static` costs nothing at run time and is right when the iterations take
similar time. The others pay for scheduling in exchange for balancing uneven
work.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPSchedule](OMPSchedule.md), [OMPRegion](../statement_nodes/OMPRegion.md)
