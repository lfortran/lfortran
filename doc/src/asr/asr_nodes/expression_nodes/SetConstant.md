# SetConstant

A set value built from its elements.

## Declaration

### Syntax

```text
SetConstant(expr* elements, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `elements` | the elements, in order. |
| `type` | the set type of the result. |

### Return values

The value of the expression.

## Description

`{a, b}`. Duplicate elements collapse, so the set may hold fewer elements than
were written.

## Examples

```clojure
(SetConstant
  :elements [
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
  ]
  :type (Set
    :type (Integer
      :kind 4
    )
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/set_expr.asr
:language: clojure
```

## See Also

[Set](../type_nodes/Set.md), [SetInsert](../statement_nodes/SetInsert.md), [SetContains](SetContains.md)
