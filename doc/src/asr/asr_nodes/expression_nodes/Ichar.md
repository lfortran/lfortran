# Ichar

The character code of a character, in the processor collating sequence.

## Declaration

### Syntax

```text
Ichar(expr arg, ttype type, expr? value)
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

`ichar(c)`. The collating sequence is the processor's; [Iachar](Iachar.md) is
the ASCII one, and the two agree wherever the processor uses ASCII.

## Examples

```clojure
(Ichar
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

[Iachar](Iachar.md), [StringOrd](StringOrd.md), [StringChr](StringChr.md)
