# ListItem

One element of a list.

## Declaration

### Syntax

```text
ListItem(expr a, expr pos, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the list. |
| `pos` | the index. |
| `type` | the element type. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a[i]`, counting from zero. Reading past the end is an error at run time.

## Examples

```clojure
(ListItem
  :a (Var
    :v (SymbolRef 1 "a")
  )
  :pos (IntegerConstant
    :n 0
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

[ListSection](ListSection.md), [ListLen](ListLen.md), [ArrayItem](ArrayItem.md)
