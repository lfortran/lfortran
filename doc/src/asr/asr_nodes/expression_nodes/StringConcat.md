# StringConcat

Concatenation of two strings.

## Declaration

### Syntax

```text
StringConcat(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `right` | the right operand. |
| `type` | the type of the result. Its length is the sum of the operand lengths when both are known. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a // b`. The result is a new value; neither operand is modified.

## Examples

```clojure
(StringConcat
  :left (Var
    :v (SymbolRef 1 "s")
  )
  :right (StringConstant
    :s "!"
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
  )
  :type (String
    :kind 1
    :len nil
    :len_kind :DeferredLength
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

[StringRepeat](StringRepeat.md), [StringSection](StringSection.md), [OverloadedStringConcat](OverloadedStringConcat.md)
