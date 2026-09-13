# StringOrd

The character code of the first character of a string.

## Declaration

### Syntax

```text
StringOrd(expr arg, ttype type, expr? value)
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

The result is an integer. [Ichar](Ichar.md) and [Iachar](Iachar.md) are the
Fortran spellings of the same idea, in the processor collating sequence and in
ASCII respectively; **StringOrd** is LPython's `ord`.

## Examples

```clojure
(StringOrd
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

[StringChr](StringChr.md), [Ichar](Ichar.md), [Iachar](Iachar.md)
