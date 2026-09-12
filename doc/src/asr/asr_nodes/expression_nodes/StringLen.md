# StringLen

The length of a string.

## Declaration

### Syntax

```text
StringLen(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the string. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`len(s)`. For a string whose length is known at compile time the frontend
folds it into `value`; for a deferred length string it is read from the
descriptor at run time.

## Examples

```clojure
(StringLen
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

```{literalinclude} ../../examples/string_expr.asr
:language: clojure
```

## See Also

[StringItem](StringItem.md), [StringSection](StringSection.md), [String](../type_nodes/String.md)
