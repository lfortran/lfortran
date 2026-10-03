# DictContains

Whether a dictionary holds a key.

## Declaration

### Syntax

```text
DictContains(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the dictionary. |
| `right` | the key looked for. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`k in d`. Only the keys are searched.

## Examples

```clojure
(DictContains
  :left (Var
    :v (SymbolRef 1 "d")
  )
  :right (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Logical
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

[DictItem](DictItem.md), [SetContains](SetContains.md), [ListContains](ListContains.md)
