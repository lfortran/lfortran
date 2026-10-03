# TupleItem

One element of a tuple.

## Declaration

### Syntax

```text
TupleItem(expr a, expr pos, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the tuple. |
| `pos` | the index. |
| `type` | the type of that element. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`t[i]`, counting from zero. The index must be a constant, because the type of
the result depends on it.

## Examples

```clojure
(TupleItem
  :a (Var
    :v (SymbolRef 1 "t")
  )
  :pos (IntegerConstant
    :n 0
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Integer
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

[Tuple](../type_nodes/Tuple.md), [TupleLen](TupleLen.md), [ListItem](ListItem.md)
