# string_physical_type

How a string is represented in memory.

## Declaration

### Syntax

```text
string_physical_type = DescriptorString | CChar
```

### Values

| Value | Meaning |
|----------|-------------|
| `DescriptorString` | a descriptor, `{char* data, int64 size, int64 capacity}`. The length and the capacity travel with the data, so the value can be reallocated and its length asked for. |
| `CChar` | a bare `char*`, the representation C uses and the one the string runtime functions take. |

### Return values

None. An enumeration value is not evaluated.

## Description

The physical type is separate from the logical type: both representations hold
the same characters.
[StringPhysicalCast](../expression_nodes/StringPhysicalCast.md) moves between
them, taking the `data` pointer out of a descriptor in one direction and
wrapping a pointer in a descriptor with `size` and `capacity` set to `-1` in
the other, marking a string that must not be extended.

A local variable may not have the `CChar` physical type: the verifier rejects
it, because a local string owns its storage and needs a descriptor to describe
it.

## See Also

[String](String.md), [StringPhysicalCast](../expression_nodes/StringPhysicalCast.md), [array_physical_type](../enum_nodes/array_physical_type.md)
