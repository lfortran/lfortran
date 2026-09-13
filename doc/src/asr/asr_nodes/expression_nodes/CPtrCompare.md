# CPtrCompare

A comparison of two C pointers.

## Declaration

### Syntax

```text
CPtrCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

`c_associated(a, b)` and comparisons against `c_null_ptr` become this node.
Only `Eq` and `NotEq` are meaningful: addresses of unrelated objects have no
useful order.

## Examples

```clojure
(CPtrCompare
  :left (Var
    :v (SymbolRef 1 "c")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 1 "c")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[CPtr](../type_nodes/CPtr.md), [CLoc](CLoc.md), [PointerAssociated](PointerAssociated.md)
