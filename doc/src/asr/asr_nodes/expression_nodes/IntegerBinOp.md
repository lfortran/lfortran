# IntegerBinOp

An arithmetic or bitwise operation on integers.

## Declaration

### Syntax

```text
IntegerBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `op` | the operator; see [binop](../enum_nodes/binop.md). |
| `right` | the right operand. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Both operands and the result have the same integer type: ASR never mixes
kinds, so `i8 + i4` is an **IntegerBinOp** on two `integer(8)` operands with a
[Cast](Cast.md) around the second.

The bitwise operators (`BitAnd`, `BitOr`, `BitXor`, `BitLShift`, `BitRShift`,
`LBitRShift`) share this node with the arithmetic ones because they take the
same operands and produce the same type.

## Examples

```clojure
(IntegerBinOp
  :left (Var
    :v (SymbolRef 1 "i")
  )
  :op :Mul
  :right (IntegerConstant
    :n 2
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/integer_expr.asr
:language: clojure
```

## See Also

[binop](../enum_nodes/binop.md), [RealBinOp](RealBinOp.md), [IntegerCompare](IntegerCompare.md), [Cast](Cast.md)
