# EnumConstructor

An enumeration value built from an integer.

## Declaration

### Syntax

```text
EnumConstructor(symbol dt_sym, expr* args, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `dt_sym` | the [Enum](../symbol_nodes/Enum.md) being constructed. |
| `args` | the value to convert, a single integer expression. |
| `type` | the [EnumType](../type_nodes/EnumType.md) of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`Color(1)` in LPython converts an integer into a value of the enumeration.
Fortran has no such conversion: an enumerator there is an ordinary named
constant, so the Fortran frontend never produces this node.

There is no example on this page: an **EnumConstructor** cannot currently be
printed as ASR text, because the type encoding used to name the operation has
no case for [EnumType](../type_nodes/EnumType.md).

## See Also

[Enum](../symbol_nodes/Enum.md), [EnumType](../type_nodes/EnumType.md), [EnumValue](EnumValue.md), [EnumName](EnumName.md)
