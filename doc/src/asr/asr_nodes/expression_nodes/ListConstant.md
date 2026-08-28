# ListConstant

A list value built from its elements.

## Declaration

### Syntax

```text
ListConstant(expr* args, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `args` | the elements, in order. |
| `type` | the list type of the result. |

### Return values

The value of the expression.

## Description

LPython's `[a, b, c]`. Unlike an
[ArrayConstructor](ArrayConstructor.md) the result has no shape and can grow:
a list is a sequence whose length changes at run time.

## Examples

```clojure
(ListConstant
  :args [
    (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    (IntegerConstant
      :n 2
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    (IntegerConstant
      :n 3
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  ]
  :type (List
    :type (Integer
      :kind 4
    )
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[List](../type_nodes/List.md), [ListItem](ListItem.md), [ListAppend](../statement_nodes/ListAppend.md), [ArrayConstructor](ArrayConstructor.md)
