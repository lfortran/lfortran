# RealConstant

A real literal.

## Declaration

### Syntax

```text
RealConstant(float r, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `r` | the value. |
| `type` | the real type, which fixes the kind and therefore the precision. |

### Return values

The value of the expression.

## Description

The value is stored as a number. ASR text prints it with enough digits to
round-trip exactly, and uses the `#asr/float64` and `#asr/real128` tags for
values a decimal literal cannot represent, such as infinities and NaNs.

## Examples

```clojure
(RealConstant
  :r 2.5
  :type (Real
    :kind 8
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/real_expr.asr
:language: clojure
```

## See Also

[IntegerConstant](IntegerConstant.md), [ComplexConstant](ComplexConstant.md), [Real](../type_nodes/Real.md)
