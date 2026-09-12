# TupleCompare

A comparison of two tuples.

## Declaration

### Syntax

```text
TupleCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

Tuples are compared element by element from the left.

## Examples

```clojure
(TupleCompare
  :left (Var
    :v (SymbolRef 1 "t")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 1 "t")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/tuple_expr.asr
:language: clojure
```

## See Also

[Tuple](../type_nodes/Tuple.md), [ListCompare](ListCompare.md)
