# ComplexConstructor

A complex value built from two real expressions.

## Declaration

### Syntax

```text
ComplexConstructor(expr re, expr im, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `re` | the real part. |
| `im` | the imaginary part. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`(a, b)` where `a` and `b` are expressions rather than literals. When both
fold to constants the frontend puts the resulting
[ComplexConstant](ComplexConstant.md) in `value`.

## Examples

```clojure
(ComplexConstructor
  :re (RealConstant
    :r 1.0
    :type (Real
      :kind 4
    )
  )
  :im (Var
    :v (SymbolRef 1 "x")
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

[ComplexConstant](ComplexConstant.md), [ComplexRe](ComplexRe.md), [ComplexIm](ComplexIm.md)
