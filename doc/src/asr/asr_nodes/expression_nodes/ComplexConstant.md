# ComplexConstant

A complex literal.

## Declaration

### Syntax

```text
ComplexConstant(float re, float im, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `re` | the real part. |
| `im` | the imaginary part. |
| `type` | the complex type, which fixes the kind. |

### Return values

The value of the expression.

## Description

Both parts are stored as numbers. A complex literal whose parts are not both
constants is a [ComplexConstructor](ComplexConstructor.md) instead.

## Examples

```clojure
(ComplexConstant
  :re 1.0
  :im 2.0
  :type (Complex
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/complex_expr.asr
:language: clojure
```

## See Also

[ComplexConstructor](ComplexConstructor.md), [RealConstant](RealConstant.md), [Complex](../type_nodes/Complex.md)
