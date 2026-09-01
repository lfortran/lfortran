# ArrayRank

The rank of an array.

## Declaration

### Syntax

```text
ArrayRank(expr v, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the array. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`rank(a)`. The rank is a property of the type, so the result is a compile time
constant except for an assumed-rank dummy argument, whose rank is only known
at the call.

## Examples

```clojure
(ArrayRank
  :v (Var
    :v (SymbolRef 1 "a")
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArraySize](ArraySize.md), [SelectRank](../statement_nodes/SelectRank.md), [Array](../type_nodes/Array.md)
