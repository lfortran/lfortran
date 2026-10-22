# pointer_bounds_form

Whether a pointer assignment gives the bounds of the pointer, and how.

## Declaration

### Syntax

```text
pointer_bounds_form = NoPointerBounds | PointerBoundsSpec | PointerBoundsRemapping
```

### Values

| Value | Meaning |
|----------|-------------|
| `NoPointerBounds` | the target is the pointer itself, `p => a`, or not a pointer. |
| `PointerBoundsSpec` | a bounds-spec-list, `p(lb:) => a`: only the lower bounds are given, and the pointer has the extents of the value. |
| `PointerBoundsRemapping` | a bounds-remapping-list, `p(lb:ub) => a`: both bounds are given, and the pointer has the elements of the value in array element order. |

### Return values

None. An enumeration value is not evaluated.

## Description

The `form` member of [Associate](../statement_nodes/Associate.md). With a
bounds-spec the pointer keeps the shape and the strides of the value, so the
upper bound of dimension `i` is `lb + size(value, i) - 1` and is not stored;
with a bounds-remapping-list it takes the elements of the value in array
element order, which is why that value has rank one or is simply contiguous.

## See Also

[Associate](../statement_nodes/Associate.md), [ArraySection](../expression_nodes/ArraySection.md)
