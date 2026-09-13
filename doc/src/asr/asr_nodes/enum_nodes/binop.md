# binop

The arithmetic and bitwise binary operators.

## Declaration

### Syntax

```text
binop
    = Add
    | Sub
    | Mul
    | Div
    | Pow
    | BitAnd
    | BitOr
    | BitXor
    | BitLShift
    | BitRShift
    | LBitRShift
```

### Values

| Value | Meaning |
|----------|-------------|
| `Add` | addition. |
| `Sub` | subtraction. |
| `Mul` | multiplication. |
| `Div` | division. For integers it truncates towards zero. |
| `Pow` | exponentiation. |
| `BitAnd` | bitwise and. |
| `BitOr` | bitwise or. |
| `BitXor` | bitwise exclusive or. |
| `BitLShift` | left shift. |
| `BitRShift` | arithmetic right shift, which keeps the sign. |
| `LBitRShift` | logical right shift, which shifts in zeros. |

### Return values

None. An enumeration value is not evaluated.

## Description

One enumeration serves [IntegerBinOp](../expression_nodes/IntegerBinOp.md),
[RealBinOp](../expression_nodes/RealBinOp.md),
[ComplexBinOp](../expression_nodes/ComplexBinOp.md) and
[UnsignedIntegerBinOp](../expression_nodes/UnsignedIntegerBinOp.md). Which
operators are meaningful depends on the node: the bitwise values are only
valid for the integer nodes, and only `Add` through `Pow` are valid for reals
and complex values.

## See Also

[IntegerBinOp](../expression_nodes/IntegerBinOp.md), [RealBinOp](../expression_nodes/RealBinOp.md), [ComplexBinOp](../expression_nodes/ComplexBinOp.md), [cmpop](cmpop.md)
