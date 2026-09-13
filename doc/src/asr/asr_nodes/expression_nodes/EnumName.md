# EnumName

The name of an enumeration value.

## Declaration

### Syntax

```text
EnumName(expr v, ttype enum_type, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the enumeration value. |
| `enum_type` | the enumeration the value belongs to. |
| `type` | the string type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The result is the spelling of the enumerator, as a string. It requires the
value to be one of the enumerators; a value built from an integer that names
none of them has no name.

## Examples

```clojure
(EnumName
  :v (Var
    :v (SymbolRef 3 "c")
  )
  :enum_type (EnumType
    :enum_type (SymbolRef 3 "color")
  )
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
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/enum_expr.asr
:language: clojure
```

## See Also

[EnumValue](EnumValue.md), [Enum](../symbol_nodes/Enum.md), [EnumType](../type_nodes/EnumType.md)
