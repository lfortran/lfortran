# StringChr

The one character string with a given character code.

## Declaration

### Syntax

```text
StringChr(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the character code. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The inverse of [StringOrd](StringOrd.md), and LPython's `chr`. Fortran spells
it `char` and `achar`.

## Examples

```clojure
(StringChr
  :arg (IntegerConstant
    :n 65
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (String
    :kind 1
    :len (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :len_kind :ExpressionLength
    :physical_type :DescriptorString
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/string_expr.asr
:language: clojure
```

## See Also

[StringOrd](StringOrd.md), [Ichar](Ichar.md)
