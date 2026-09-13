# ListSection

A slice of a list.

## Declaration

### Syntax

```text
ListSection(expr a, array_index section, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the list. |
| `section` | the [array_index](../helper_nodes/array_index.md) giving the start, the end and the stride. |
| `type` | the list type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a[i:j:k]`. The result is a new list holding the selected elements.

## Examples

```clojure
(ListSection
  :a (Var
    :v (SymbolRef 1 "a")
  )
  :section (array_index
    :left (IntegerConstant
      :n 0
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :right (IntegerConstant
      :n 2
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :step (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  )
  :type (List
    :type (Integer
      :kind 4
    )
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[ListItem](ListItem.md), [ArraySection](ArraySection.md), [array_index](../helper_nodes/array_index.md)
