# RealSqrt

The square root of a real.

## Declaration

### Syntax

```text
RealSqrt(expr arg, ttype type, expr? value)
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

`sqrt` has a node of its own because most targets have a square root
instruction, so lowering it as an ordinary intrinsic call would lose that.

## Examples

```clojure
(RealSqrt
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

[RealBinOp](RealBinOp.md), [IntrinsicElementalFunction](IntrinsicElementalFunction.md)
