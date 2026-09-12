# TupleContains

Whether a tuple contains a value.

## Declaration

### Syntax

```text
TupleContains(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the tuple. |
| `right` | the value looked for. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`x in t`.

## Examples

```clojure
(TupleContains
  :left (Var
    :v (SymbolRef 1 "t")
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

```{literalinclude} ../../examples/tuple_expr.asr
:language: clojure
```

## See Also

[TupleItem](TupleItem.md), [ListContains](ListContains.md)
