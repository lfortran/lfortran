# UnsignedIntegerBinOp

An arithmetic or bitwise operation on unsigned integers.

## Declaration

### Syntax

```text
UnsignedIntegerBinOp(expr left, binop op, expr right, ttype type,
    expr? value)
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

The unsigned counterpart of [IntegerBinOp](IntegerBinOp.md). Division and the
right shift differ from the signed operations, which is why the two cannot
share a node.

## Examples

```clojure
(UnsignedIntegerBinOp
  :left (Var
    :v (SymbolRef 1 "u")
  )
  :op :Add
  :right (UnsignedIntegerConstant
    :n 1
    :type (UnsignedInteger
      :kind 4
    )
  )
  :type (UnsignedInteger
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/unsigned_expr.asr
:language: clojure
```

## See Also

[IntegerBinOp](IntegerBinOp.md), [UnsignedIntegerCompare](UnsignedIntegerCompare.md), [UnsignedInteger](../type_nodes/UnsignedInteger.md)
