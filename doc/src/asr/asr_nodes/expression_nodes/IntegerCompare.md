# IntegerCompare

A comparison of two integers.

## Declaration

### Syntax

```text
IntegerCompare(expr left, cmpop op, expr right, ttype type,
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

The operands are of the same integer type and the result is logical, so the
type of the node is a [Logical](../type_nodes/Logical.md), not an integer.

## Examples

```clojure
(IntegerCompare
  :left (Var
    :v (SymbolRef 1 "i")
  )
  :op :Lt
  :right (Var
    :v (SymbolRef 1 "j")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/integer_expr.asr
:language: clojure
```

## See Also

[cmpop](../enum_nodes/cmpop.md), [RealCompare](RealCompare.md), [IntegerBinOp](IntegerBinOp.md)
