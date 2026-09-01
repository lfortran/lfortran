# EnumType

The type of an enumeration value.

## Declaration

### Syntax

```text
EnumType(symbol enum_type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `enum_type` | the [Enum](../symbol_nodes/Enum.md) symbol that defines the enumeration. |

### Return values

None. A type is not evaluated.

## Description

Unlike [StructType](StructType.md) this type carries nothing but the symbol,
because everything about an enumeration, including the integer type its values
are stored in, is in the definition.

In Fortran an enumerator is an ordinary integer named constant, so a Fortran
program has enumerator constants but rarely a variable of **EnumType**; it is
LPython's `Enum` that uses the type directly.

## Examples

```clojure
(EnumType
  :enum_type (SymbolRef 3 "color")
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/enum_expr.asr
:language: clojure
```

## See Also

[Enum](../symbol_nodes/Enum.md), [EnumValue](../expression_nodes/EnumValue.md), [EnumName](../expression_nodes/EnumName.md), [Integer](Integer.md)
