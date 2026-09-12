# TupleConstant

A tuple value built from its elements.

## Declaration

### Syntax

```text
TupleConstant(expr* elements, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `elements` | the elements, in order. |
| `type` | the tuple type of the result. |

### Return values

The value of the expression.

## Description

`(a, b)`. A tuple has a fixed length and its elements may have different
types, which is what distinguishes it from a [List](../type_nodes/List.md).

## Examples

```clojure
(TupleConstant
  :elements [
    (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    (RealConstant
      :r 2.0
      :type (Real
        :kind 4
      )
    )
  ]
  :type (Tuple
    :type [
      (Integer
        :kind 4
      )
      (Real
        :kind 4
      )
    ]
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/tuple_expr.asr
:language: clojure
```

## See Also

[Tuple](../type_nodes/Tuple.md), [TupleItem](TupleItem.md), [ListConstant](ListConstant.md)
