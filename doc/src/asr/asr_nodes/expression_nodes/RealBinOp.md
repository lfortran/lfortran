# RealBinOp

An arithmetic operation on reals.

## Declaration

### Syntax

```text
RealBinOp(expr left, binop op, expr right, ttype type, expr? value)
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

Both operands and the result have the same real type. There are no bitwise
operators here: `binop` values such as `BitAnd` are not valid for reals.

## Examples

```clojure
(RealBinOp
  :left (Var
    :v (SymbolRef 1 "x")
  )
  :op :Div
  :right (RealConstant
    :r 2.0
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

[binop](../enum_nodes/binop.md), [IntegerBinOp](IntegerBinOp.md), [RealCompare](RealCompare.md)
