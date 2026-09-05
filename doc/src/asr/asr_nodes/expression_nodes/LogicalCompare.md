# LogicalCompare

A comparison of two logical values.

## Declaration

### Syntax

```text
LogicalCompare(expr left, cmpop op, expr right, ttype type,
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

Only `Eq` and `NotEq` are meaningful. `p == q` on logicals is the same
operation as `p .eqv. q`; the two spellings produce different nodes so that
unparsing can reproduce what was written.

## Examples

```clojure
(LogicalCompare
  :left (Var
    :v (SymbolRef 1 "p")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 1 "q")
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

[cmpop](../enum_nodes/cmpop.md), [LogicalBinOp](LogicalBinOp.md)
