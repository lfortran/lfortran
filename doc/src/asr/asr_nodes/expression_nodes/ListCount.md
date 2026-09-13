# ListCount

How many times a value occurs in a list.

## Declaration

### Syntax

```text
ListCount(expr arg, expr ele, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the list. |
| `ele` | the value to count. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a.count(x)`. Every element is compared with the value.

## Examples

```clojure
(ListCount
  :arg (Var
    :v (SymbolRef 1 "a")
  )
  :ele (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
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

[ListContains](ListContains.md), [ListItem](ListItem.md), [List](../type_nodes/List.md)
