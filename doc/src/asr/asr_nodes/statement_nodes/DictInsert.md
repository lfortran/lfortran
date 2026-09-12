# DictInsert

Stores a value under a key in a dictionary.

## Declaration

### Syntax

```text
DictInsert(expr a, expr key, expr value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the dictionary. |
| `key` | the key. |
| `value` | the value to store. |

### Return values

None.

## Description

This is `d[k] = v`. An existing entry for the key is replaced, so the
dictionary grows only when the key is new.

## Examples

```clojure
(DictInsert
  :a (Var
    :v (SymbolRef 1 "d")
  )
  :key (IntegerConstant
    :n 2
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :value (RealConstant
    :r 2.5
    :type (Real
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

[DictClear](DictClear.md), [DictItem](../expression_nodes/DictItem.md), [DictPop](../expression_nodes/DictPop.md), [Dict](../type_nodes/Dict.md)
