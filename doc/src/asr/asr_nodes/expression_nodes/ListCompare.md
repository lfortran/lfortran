# ListCompare

A comparison of two lists.

## Declaration

### Syntax

```text
ListCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

Lists are compared element by element. The result is logical.

## Examples

```clojure
(ListCompare
  :left (Var
    :v (SymbolRef 1 "a")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 1 "b")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[List](../type_nodes/List.md), [TupleCompare](TupleCompare.md), [ListContains](ListContains.md)
