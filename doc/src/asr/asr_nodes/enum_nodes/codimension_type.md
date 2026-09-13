# codimension_type

Whether a codimension bound is an expression or `*`.

## Declaration

### Syntax

```text
codimension_type = CodimensionExpr | CodimensionStar
```

### Values

| Value | Meaning |
|----------|-------------|
| `CodimensionExpr` | the bound is the expression stored beside it. |
| `CodimensionStar` | the bound is `*`, so the extent is decided by the number of images at run time. |

### Return values

None. An enumeration value is not evaluated.

## Description

The last codimension of a coarray is always `*`, because the number of images
is not known when the program is compiled.

## See Also

[codimension](../helper_nodes/codimension.md), [coarray_index](../helper_nodes/coarray_index.md), [CoarrayRef](../expression_nodes/CoarrayRef.md)
