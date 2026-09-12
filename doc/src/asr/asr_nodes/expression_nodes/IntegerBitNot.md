# IntegerBitNot

Bitwise complement of an integer.

## Declaration

### Syntax

```text
IntegerBitNot(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the operand. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

**IntegerBitNot** is `not(i)`: every bit of the operand is flipped. It is a
separate node from [IntegerUnaryMinus](IntegerUnaryMinus.md) because the two
differ by one for two's complement integers, and because the bitwise operation
is meaningless for the other numeric types.

## Examples

```clojure
(IntegerBitNot
  :arg (Var
    :v (SymbolRef 1 "i")
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

[IntegerUnaryMinus](IntegerUnaryMinus.md), [IntegerBinOp](IntegerBinOp.md), [IntegerBitLen](IntegerBitLen.md)
