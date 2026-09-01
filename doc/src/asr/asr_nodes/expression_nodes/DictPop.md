# DictPop

Removes an entry and returns its value.

## Declaration

### Syntax

```text
DictPop(expr a, expr key, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the dictionary. |
| `key` | the key to remove. |
| `type` | the value type of the dictionary. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`d.pop(k)`. Removing a key the dictionary does not hold is an error at run
time.

## Examples

```clojure
(DictPop
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

[DictItem](DictItem.md), [DictClear](../statement_nodes/DictClear.md), [SetPop](SetPop.md)
