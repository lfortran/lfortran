# IntegerUnaryMinus

Negation of an integer.

## Declaration

### Syntax

```text
IntegerUnaryMinus(expr arg, ttype type, expr? value)
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

The result has the type of the operand. Negating the most negative value of a
kind overflows, and ASR does not diagnose it.

## Examples

```clojure
(IntegerUnaryMinus
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

[IntegerBinOp](IntegerBinOp.md), [RealUnaryMinus](RealUnaryMinus.md)
