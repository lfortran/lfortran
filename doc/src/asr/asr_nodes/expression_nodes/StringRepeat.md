# StringRepeat

A string repeated a number of times.

## Declaration

### Syntax

```text
StringRepeat(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the string to repeat. |
| `right` | how many times to repeat it. |
| `type` | the type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

This is `repeat(s, n)`. A count of zero gives the empty string.

## Examples

```clojure
(StringRepeat
  :left (Var
    :v (SymbolRef 1 "s")
  )
  :right (IntegerConstant
    :n 3
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
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

[StringConcat](StringConcat.md), [StringSection](StringSection.md)
