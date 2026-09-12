# UnsignedIntegerBitNot

Bitwise complement of an unsigned integer.

## Declaration

### Syntax

```text
UnsignedIntegerBitNot(expr arg, ttype type, expr? value)
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

Every bit of the operand is flipped.

## Examples

```clojure
(UnsignedIntegerBitNot
  :arg (Var
    :v (SymbolRef 1 "u")
  )
  :type (UnsignedInteger
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/unsigned_expr.asr
:language: clojure
```

## See Also

[IntegerBitNot](IntegerBitNot.md), [UnsignedIntegerBinOp](UnsignedIntegerBinOp.md)
