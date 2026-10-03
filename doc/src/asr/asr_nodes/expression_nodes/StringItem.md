# StringItem

One character of a string.

## Declaration

### Syntax

```text
StringItem(expr arg, expr idx, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the string. |
| `idx` | the index, counting from one. |
| `type` | the type of the result: a string of length one. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`s(i:i)` written as a single index. The result is a string of length one, not
a character code; [StringOrd](StringOrd.md) is what gives the code.

## Examples

```clojure
(StringItem
  :arg (Var
    :v (SymbolRef 1 "s")
  )
  :idx (IntegerConstant
    :n 1
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

[StringSection](StringSection.md), [StringOrd](StringOrd.md), [StringLen](StringLen.md)
