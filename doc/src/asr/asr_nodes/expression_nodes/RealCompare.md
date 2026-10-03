# RealCompare

A comparison of two reals.

## Declaration

### Syntax

```text
RealCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

The result is logical. Comparing a NaN with anything is false, including with
itself, and ASR leaves that to the hardware rather than folding it.

## Examples

```clojure
(RealCompare
  :left (Var
    :v (SymbolRef 1 "x")
  )
  :op :GtE
  :right (Var
    :v (SymbolRef 1 "y")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/real_expr.asr
:language: clojure
```

## See Also

[cmpop](../enum_nodes/cmpop.md), [IntegerCompare](IntegerCompare.md), [RealBinOp](RealBinOp.md)
