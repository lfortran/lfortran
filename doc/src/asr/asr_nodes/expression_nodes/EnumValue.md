# EnumValue

The integer value of an enumeration value.

## Declaration

### Syntax

```text
EnumValue(expr v, ttype enum_type, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the enumeration value. |
| `enum_type` | the enumeration the value belongs to. |
| `type` | the integer type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The inverse of [EnumConstructor](EnumConstructor.md): it takes the
enumeration value back to the integer it is stored as. In Fortran an
enumerator is already an integer constant, so nothing is needed there.

## Examples

```clojure
(EnumValue
  :v (Var
    :v (SymbolRef 3 "c")
  )
  :enum_type (EnumType
    :enum_type (SymbolRef 3 "color")
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/enum_expr.asr
:language: clojure
```

## See Also

[EnumName](EnumName.md), [EnumConstructor](EnumConstructor.md), [Enum](../symbol_nodes/Enum.md)
