# ListRepeat

A list repeated a number of times.

## Declaration

### Syntax

```text
ListRepeat(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the list. |
| `right` | the repeat count. |
| `type` | the list type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a * n`. The result holds the elements of the list `n` times over.

## Examples

```clojure
(ListRepeat
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

[ListConcat](ListConcat.md), [StringRepeat](StringRepeat.md)
