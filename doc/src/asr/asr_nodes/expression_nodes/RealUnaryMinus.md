# RealUnaryMinus

Negation of a real.

## Declaration

### Syntax

```text
RealUnaryMinus(expr arg, ttype type, expr? value)
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

The sign bit of the operand is flipped, so negating zero gives negative zero.

## Examples

```clojure
(RealUnaryMinus
  :arg (Var
    :v (SymbolRef 1 "x")
  )
  :type (Real
    :kind 8
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/real_expr.asr
:language: clojure
```

## See Also

[IntegerUnaryMinus](IntegerUnaryMinus.md), [RealBinOp](RealBinOp.md)
