# array_physical_type

How an array is represented in memory.

## Declaration

### Syntax

```text
array_physical_type
    = DescriptorArray
    | PointerArray
    | UnboundedPointerArray
    | FixedSizeArray
    | StringArraySinglePointer
    | SIMDArray
    | AssumedRankArray
```

### Values

| Value | Meaning |
|----------|-------------|
| `DescriptorArray` | a descriptor carrying the data pointer, the bounds and the strides. Any array can be described this way, including a non-contiguous section. |
| `PointerArray` | a bare pointer to contiguous data, with the shape known from the type. |
| `UnboundedPointerArray` | a bare pointer with no extent at all, as for an assumed size dummy argument. A local variable of this type is a view of elements that it never owns: its declaration does not initialize them, and leaving its scope does not finalize or deallocate them. |
| `FixedSizeArray` | storage whose extents are compile time constants, held inline rather than behind a pointer. |
| `StringArraySinglePointer` | an array of strings held as one allocation. |
| `SIMDArray` | a short fixed length array held in a vector register. |
| `AssumedRankArray` | `dimension(..)`: the rank itself is not known until the call. Such a type has no dimensions. |

### Return values

None. An enumeration value is not evaluated.

## Description

The physical type is not the logical type: two arrays with the same element
type, rank and extents may be represented differently, and
[ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md) is what converts
between the representations without changing the value.

Passing a `FixedSizeArray` to a procedure expecting a `DescriptorArray` is the
common case: the cast builds the descriptor. The result of a cast to any
physical type other than `DescriptorArray` is a plain array value, never an
allocatable or a pointer, since only a descriptor can hold bounds that change
at run time.

[Array](../type_nodes/Array.md) lists the rules the verifier checks between the
physical type and the dimensions.

## See Also

[Array](../type_nodes/Array.md), [ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md), [ArrayIsContiguous](../expression_nodes/ArrayIsContiguous.md), [SelectRank](../statement_nodes/SelectRank.md)
