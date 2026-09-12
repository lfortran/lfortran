# cmpop

The comparison operators.

## Declaration

### Syntax

```text
cmpop = Eq | NotEq | Lt | LtE | Gt | GtE
```

### Values

| Value | Meaning |
|----------|-------------|
| `Eq` | equal. |
| `NotEq` | not equal. |
| `Lt` | less than. |
| `LtE` | less than or equal. |
| `Gt` | greater than. |
| `GtE` | greater than or equal. |

### Return values

None. An enumeration value is not evaluated.

## Description

Shared by every comparison node. Only `Eq` and `NotEq` are meaningful for
complex values, logicals and C pointers, which have no order.

## See Also

[IntegerCompare](../expression_nodes/IntegerCompare.md), [RealCompare](../expression_nodes/RealCompare.md), [StringCompare](../expression_nodes/StringCompare.md), [binop](binop.md)
