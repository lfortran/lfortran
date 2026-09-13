# LogicalBinOp

A logical operation on two logical values.

## Declaration

### Syntax

```text
LogicalBinOp(expr left, logicalbinop op, expr right, ttype type,
    expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `op` | the operator; see [logicalbinop](../enum_nodes/logicalbinop.md). |
| `right` | the right operand. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`And`, `Or`, `Xor`, `Eqv` and `NEqv` are the operators; see
[logicalbinop](../enum_nodes/logicalbinop.md). ASR does not promise short
circuit evaluation: `.and.` in Fortran may evaluate both operands.

## Examples

```clojure
(LogicalBinOp
  :left (Var
    :v (SymbolRef 1 "p")
  )
  :op :Or
  :right (LogicalConstant
    :value false
    :type (Logical
      :kind 4
    )
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/logical_expr.asr
:language: clojure
```

## See Also

[logicalbinop](../enum_nodes/logicalbinop.md), [LogicalNot](LogicalNot.md), [LogicalCompare](LogicalCompare.md)
