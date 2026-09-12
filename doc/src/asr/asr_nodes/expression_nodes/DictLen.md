# DictLen

The number of entries of a dictionary.

## Declaration

### Syntax

```text
DictLen(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the dictionary. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`len(d)`.

## Examples

```clojure
(DictLen
  :arg (Var
    :v (SymbolRef 1 "d")
  )
  :type (Integer
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

[Dict](../type_nodes/Dict.md), [SetLen](SetLen.md)
