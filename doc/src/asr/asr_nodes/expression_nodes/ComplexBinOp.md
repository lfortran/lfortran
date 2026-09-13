# ComplexBinOp

An arithmetic operation on complex values.

## Declaration

### Syntax

```text
ComplexBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `op` | the operator; see [binop](../enum_nodes/binop.md). |
| `right` | the right operand. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Only `Add`, `Sub`, `Mul`, `Div` and `Pow` are meaningful here. Both operands
and the result have the same complex type.

## Examples

```clojure
(ComplexBinOp
  :left (Var
    :v (SymbolRef 1 "z")
  )
  :op :Mul
  :right (ComplexConstant
    :re 0.0
    :im 1.0
    :type (Complex
      :kind 4
    )
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

[binop](../enum_nodes/binop.md), [RealBinOp](RealBinOp.md), [ComplexCompare](ComplexCompare.md)
