# Iachar

The ASCII code of a character.

## Declaration

### Syntax

```text
Iachar(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the string whose first character is taken. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`iachar(c)`. Unlike [Ichar](Ichar.md) the mapping is fixed by the standard, so
the result does not depend on the processor.

## Examples

```clojure
(Iachar
  :arg (Var
    :v (SymbolRef 1 "c")
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

[Ichar](Ichar.md), [StringOrd](StringOrd.md)
