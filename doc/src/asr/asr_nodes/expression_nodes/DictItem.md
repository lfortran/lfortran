# DictItem

The value stored under a key.

## Declaration

### Syntax

```text
DictItem(expr a, expr key, expr? default, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the dictionary. |
| `key` | the key to look up. |
| `default` | the value to produce when the key is absent, or `nil`. |
| `type` | the value type of the dictionary. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`d[k]`, and `d.get(k, default)` when `default` is given. Without a default,
looking up a key the dictionary does not hold is an error at run time.

## Examples

```clojure
(DictItem
  :a (Var
    :v (SymbolRef 1 "d")
  )
  :key (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :default (RealConstant
    :r 0.0
    :type (Real
      :kind 4
    )
  )
  :type (Real
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/dict_expr.asr
:language: clojure
```

## See Also

[DictInsert](../statement_nodes/DictInsert.md), [DictPop](DictPop.md), [DictContains](DictContains.md)
