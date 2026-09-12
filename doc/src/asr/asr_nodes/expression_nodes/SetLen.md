# SetLen

The number of elements of a set.

## Declaration

### Syntax

```text
SetLen(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the set. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`len(s)`.

## Examples

```clojure
(SetLen
  :arg (Var
    :v (SymbolRef 1 "s")
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/set_expr.asr
:language: clojure
```

## See Also

[Set](../type_nodes/Set.md), [ListLen](ListLen.md)
