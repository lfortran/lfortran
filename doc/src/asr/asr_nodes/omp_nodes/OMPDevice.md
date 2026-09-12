# OMPDevice

`device`: which device to run a target region on.

## Declaration

### Syntax

```text
OMPDevice(expr device)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `device` | the device number. |

### Return values

None.

## Description

The number identifies the device; the host is not one of them.

## Examples

```clojure
(OMPDevice
  :device (IntegerConstant
    :n 0
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

[OMPMap](OMPMap.md), [OMPNumTeams](OMPNumTeams.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
