# ComplexCompare

A comparison of two complex values.

## Declaration

### Syntax

```text
ComplexCompare(expr left, cmpop op, expr right, ttype type,
    expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `op` | the comparison; see [cmpop](../enum_nodes/cmpop.md). |
| `right` | the right operand. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Only `Eq` and `NotEq` are meaningful: complex numbers are not ordered, and a
frontend must reject `<` on them rather than emitting this node with an
ordering comparison.

## Examples

```clojure
(ComplexCompare
  :left (Var
    :v (SymbolRef 1 "z")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 1 "w")
  )
  :type (Logical
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

[cmpop](../enum_nodes/cmpop.md), [RealCompare](RealCompare.md), [ComplexBinOp](ComplexBinOp.md)
