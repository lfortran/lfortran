# ComplexRe

The real part of a complex value.

## Declaration

### Syntax

```text
ComplexRe(expr arg, ttype type, expr? value)
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

The result is a real of the kind of the operand, so `real(z)` where `z` is
`complex(8)` gives a `real(8)`.

## Examples

```clojure
(ComplexRe
  :arg (Var
    :v (SymbolRef 1 "z")
  )
  :type (Real
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

[ComplexIm](ComplexIm.md), [ComplexConstructor](ComplexConstructor.md)
