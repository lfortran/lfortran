# ComplexUnaryMinus

Negation of a complex value.

## Declaration

### Syntax

```text
ComplexUnaryMinus(expr arg, ttype type, expr? value)
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

Both parts are negated.

## Examples

```clojure
(ComplexUnaryMinus
  :arg (Var
    :v (SymbolRef 1 "z")
  )
  :type (Complex
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/complex_expr.asr
:language: clojure
```

## See Also

[ComplexBinOp](ComplexBinOp.md), [RealUnaryMinus](RealUnaryMinus.md)
