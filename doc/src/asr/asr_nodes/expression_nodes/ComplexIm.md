# ComplexIm

The imaginary part of a complex value.

## Declaration

### Syntax

```text
ComplexIm(expr arg, ttype type, expr? value)
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

The result is a real of the kind of the operand.

## Examples

```clojure
(ComplexIm
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

[ComplexRe](ComplexRe.md), [ComplexConstructor](ComplexConstructor.md)
