# RealCopySign

A real with the magnitude of one operand and the sign of another.

## Declaration

### Syntax

```text
RealCopySign(expr target, expr source, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the value whose magnitude is taken. |
| `source` | the value whose sign is taken. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

This is `sign(a, b)`. It is a node of its own rather than an intrinsic call
because it maps to a single machine instruction, and because it is generated
by the `sign_from_value` optimisation pass.

The sign is copied from the bit, so a negative zero source gives a negative
result.

## Examples

```clojure
(RealCopySign
  :target (Var
    :v (SymbolRef 1 "x")
  )
  :source (RealConstant
    :r -1.0
    :type (Real
      :kind 8
    )
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

[RealUnaryMinus](RealUnaryMinus.md), [RealSqrt](RealSqrt.md), [IntrinsicElementalFunction](IntrinsicElementalFunction.md)
