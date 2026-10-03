# ListLen

The number of elements of a list.

## Declaration

### Syntax

```text
ListLen(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the list. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`len(a)`. The length is read at run time, since it changes as the list is
appended to.

## Examples

```clojure
(ListLen
  :arg (Var
    :v (SymbolRef 1 "a")
  )
  :type (Integer
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

[List](../type_nodes/List.md), [ListItem](ListItem.md), [ArraySize](ArraySize.md)
