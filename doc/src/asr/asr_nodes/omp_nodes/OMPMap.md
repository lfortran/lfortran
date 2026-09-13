# OMPMap

`map`: how data moves between the host and a device.

## Declaration

### Syntax

```text
OMPMap(map_type type, expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the direction; see [map_type](map_type.md). |
| `vars` | the variables the clause applies to. |

### Return values

None.

## Description

A device has its own memory, so a `target` region has to say which data to
copy and in which direction. `To` copies in, `From` copies out, `ToFrom` does
both, and `Alloc`, `Release` and `Delete` manage the device copy without
transferring anything.

## Examples

```clojure
(OMPMap
  :type :ToFrom
  :vars [
    (Var
      :v (SymbolRef 1 "a")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[map_type](map_type.md), [OMPDevice](OMPDevice.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
