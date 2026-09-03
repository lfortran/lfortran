# StringConstant

A character literal.

## Declaration

### Syntax

```text
StringConstant(string s, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `s` | the bytes of the constant. |
| `type` | the string type, whose `len` is the length of the constant. |

### Return values

The value of the expression.

## Description

A Fortran character value is a byte array, not text: `achar(200)` is a valid
character whose byte is not valid UTF-8. ASR text writes such a constant as
`#asr/bytes` rather than as a string, so that a document stays decodable and
no byte is silently reinterpreted.

## Examples

```clojure
(StringConstant
  :s "hello"
  :type (String
    :kind 1
    :len (IntegerConstant
      :n 5
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :len_kind :ExpressionLength
    :physical_type :DescriptorString
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/string_expr.asr
:language: clojure
```

## See Also

[String](../type_nodes/String.md), [StringConcat](StringConcat.md), [StringLen](StringLen.md)
