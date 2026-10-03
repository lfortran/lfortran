# ListContains

Whether a list contains a value.

## Declaration

### Syntax

```text
ListContains(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the list. |
| `right` | the value looked for. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`x in a`. The result is logical.

## Examples

```clojure
(ListContains
  :left (Var
    :v (SymbolRef 1 "a")
  )
  :right (IntegerConstant
    :n 2
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

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[ListCount](ListCount.md), [SetContains](SetContains.md), [DictContains](DictContains.md)
