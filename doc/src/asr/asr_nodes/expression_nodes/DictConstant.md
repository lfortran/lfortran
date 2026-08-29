# DictConstant

A dictionary value built from its entries.

## Declaration

### Syntax

```text
DictConstant(expr* keys, expr* values, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `keys` | the keys, in order. |
| `values` | the values, in the same order. |
| `type` | the dictionary type of the result. |

### Return values

The value of the expression.

## Description

`{k: v}`. `keys` and `values` are parallel: the entry `i` maps `keys[i]` to
`values[i]`.

## Examples

```clojure
(DictConstant
  :keys [
    (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  ]
  :values [
    (RealConstant
      :r 1.5
      :type (Real
        :kind 4
      )
    )
  ]
  :type (Dict
    :key_type (Integer
      :kind 4
    )
    :value_type (Real
      :kind 4
    )
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/dict_expr.asr
:language: clojure
```

## See Also

[Dict](../type_nodes/Dict.md), [DictItem](DictItem.md), [DictInsert](../statement_nodes/DictInsert.md)
