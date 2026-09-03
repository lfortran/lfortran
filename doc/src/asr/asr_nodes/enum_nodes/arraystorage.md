# arraystorage

The order the elements of an array are stored in.

## Declaration

### Syntax

```text
arraystorage = RowMajor | ColMajor
```

### Values

| Value | Meaning |
|----------|-------------|
| `RowMajor` | the last subscript varies fastest, as in C and NumPy. |
| `ColMajor` | the first subscript varies fastest, as in Fortran. |

### Return values

None. An enumeration value is not evaluated.

## Description

Fortran arrays are `ColMajor`. The value is carried on
[ArrayConstant](../expression_nodes/ArrayConstant.md),
[ArrayConstructor](../expression_nodes/ArrayConstructor.md) and
[ArrayItem](../expression_nodes/ArrayItem.md) so that data coming from a
frontend with the other convention keeps its own order rather than being
silently transposed.

## See Also

[ArrayConstant](../expression_nodes/ArrayConstant.md), [ArrayConstructor](../expression_nodes/ArrayConstructor.md), [ArrayItem](../expression_nodes/ArrayItem.md), [Array](../type_nodes/Array.md)
