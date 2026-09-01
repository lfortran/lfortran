# TupleConcat

Two tuples joined into a new one.

## Declaration

### Syntax

```text
TupleConcat(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the first tuple. |
| `right` | the second tuple. |
| `type` | the tuple type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a + b`. The type of the result is the concatenation of the two tuple types,
so it is longer than either operand.

## Examples

```clojure
(TupleConcat
  :left (Var
    :v (SymbolRef 1 "t")
  )
  :right (Var
    :v (SymbolRef 1 "t")
  )
  :type (Tuple
    :type [
      (Integer
        :kind 4
      )
      (Real
        :kind 4
      )
      (Integer
        :kind 4
      )
      (Real
        :kind 4
      )
    ]
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/tuple_expr.asr
:language: clojure
```

## See Also

[Tuple](../type_nodes/Tuple.md), [ListConcat](ListConcat.md)
