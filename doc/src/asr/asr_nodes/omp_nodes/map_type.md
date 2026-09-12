# map_type

The directions data can move in an OpenMP `map` clause.

## Declaration

### Syntax

```text
map_type 
    = To | From | ToFrom | Alloc | Release | Delete
```

### Values

| Value | Meaning |
|----------|-------------|
| `To` | copy from the host to the device on entry. |
| `From` | copy from the device to the host on exit. |
| `ToFrom` | copy in on entry and out on exit. |
| `Alloc` | allocate on the device without copying anything in. |
| `Release` | decrement the device reference count on exit. |
| `Delete` | remove the device copy unconditionally on exit. |

### Return values

None.

## Description

The default for a variable a `target` region uses is `ToFrom`; naming a
direction explicitly avoids a copy that is not needed, which is usually the
expensive part of running on a device.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPMap](OMPMap.md), [OMPDevice](OMPDevice.md), [OMPRegion](../statement_nodes/OMPRegion.md)
