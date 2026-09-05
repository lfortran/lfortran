# cast_kind

Which conversion a [Cast](../expression_nodes/Cast.md) performs.

## Declaration

### Syntax

```text
cast_kind
    = RealToInteger
    | IntegerToReal
    | LogicalToReal
    | RealToReal
    | IntegerToInteger
    | RealToComplex
    | IntegerToComplex
    | IntegerToLogical
    | RealToLogical
    | StringToLogical
    | StringToInteger
    | StringToList
    | ComplexToLogical
    | ComplexToComplex
    | ComplexToReal
    | ComplexToInteger
    | LogicalToInteger
    | LogicalToLogical
    | RealToString
    | IntegerToString
    | LogicalToString
    | StringToString
    | UnsignedIntegerToInteger
    | UnsignedIntegerToUnsignedInteger
    | UnsignedIntegerToReal
    | UnsignedIntegerToLogical
    | IntegerToUnsignedInteger
    | RealToUnsignedInteger
    | CPtrToUnsignedInteger
    | UnsignedIntegerToCPtr
    | IntegerToSymbolicExpression
    | ListToArray
    | StringToArray
    | PointerToInteger
    | ClassToStruct
    | ClassToClass
    | ClassToIntrinsic
```

### Values

| Value | Meaning |
|----------|-------------|
| `RealToInteger` | truncates towards zero. |
| `IntegerToReal` | exact for values the real type can represent. |
| `LogicalToReal` | false becomes 0.0, true becomes 1.0. |
| `RealToReal` | changes the real kind, rounding when it narrows. |
| `IntegerToInteger` | changes the integer kind, wrapping when it narrows. |
| `RealToComplex` | the value becomes the real part; the imaginary part is zero. |
| `IntegerToComplex` | the value becomes the real part. |
| `IntegerToLogical` | zero becomes false, anything else true. |
| `RealToLogical` | zero becomes false, anything else true. |
| `StringToLogical` | an LPython conversion: the empty string is false. |
| `StringToInteger` | parses the string as an integer. |
| `StringToList` | an LPython conversion to a list of characters. |
| `ComplexToLogical` | zero becomes false. |
| `ComplexToComplex` | changes the complex kind. |
| `ComplexToReal` | takes the real part. |
| `ComplexToInteger` | takes the real part and truncates it. |
| `LogicalToInteger` | false becomes 0, true becomes 1. |
| `LogicalToLogical` | changes the logical kind. |
| `RealToString` | formats the value. |
| `IntegerToString` | formats the value. |
| `LogicalToString` | formats the value. |
| `StringToString` | changes the string kind. |
| `UnsignedIntegerToInteger` | reinterprets the value as signed. |
| `UnsignedIntegerToUnsignedInteger` | changes the unsigned kind. |
| `UnsignedIntegerToReal` | exact for values the real type can represent. |
| `UnsignedIntegerToLogical` | zero becomes false. |
| `IntegerToUnsignedInteger` | reinterprets the value as unsigned. |
| `RealToUnsignedInteger` | truncates towards zero. |
| `CPtrToUnsignedInteger` | the address as a number. |
| `UnsignedIntegerToCPtr` | a number as an address. |
| `IntegerToSymbolicExpression` | wraps an integer as a symbolic expression. |
| `ListToArray` | an LPython conversion from a list to an array. |
| `StringToArray` | a string as an array of characters. |
| `PointerToInteger` | the address as a number. |
| `ClassToStruct` | narrows a polymorphic value to a derived type. |
| `ClassToClass` | changes which class a polymorphic value is described as. |
| `ClassToIntrinsic` | narrows a polymorphic value to an intrinsic type. |

### Return values

None. An enumeration value is not evaluated.

## Description

Every implicit conversion is explicit in ASR, and this enumeration says which
conversion a [Cast](../expression_nodes/Cast.md) performs. The kind and the
`type` of the **Cast** agree: the kind names the pair of types, and the type
is the result.

Conversions that do not change the bits belong to
[ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md) and
[StringPhysicalCast](../expression_nodes/StringPhysicalCast.md) instead.

## Examples

```clojure
(Cast
  :arg (Var
    :v (SymbolRef 1 "i")
  )
  :kind :IntegerToReal
  :type (Real
    :kind 8
  )
  :value nil
  :dest nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cast_expr.asr
:language: clojure
```

## See Also

[Cast](../expression_nodes/Cast.md), [Integer](../type_nodes/Integer.md), [Real](../type_nodes/Real.md), [ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md), [StringPhysicalCast](../expression_nodes/StringPhysicalCast.md)
