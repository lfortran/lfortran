# TupleLen

The number of elements of a tuple.

## Declaration

### Syntax

```text
TupleLen(expr arg, ttype type, expr value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the tuple. |
| `type` | the type of the expression. |
| `value` | the length. It is always known, so this member is required. |

### Return values

The value of the expression.

## Description

`len(t)`. The length is part of the type, so unlike
[ListLen](ListLen.md) the answer is always known at compile time, and the
`value` member is required rather than optional.

## Examples

```clojure
(TupleLen
  :arg (Var
    :v (SymbolRef 1 "t")
  )
  :type (Integer
    :kind 4
  )
  :value (IntegerConstant
    :n 2
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/tuple_expr.asr
:language: clojure
```

## See Also

[Tuple](../type_nodes/Tuple.md), [ListLen](ListLen.md)
