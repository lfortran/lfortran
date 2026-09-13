# String

A character type.

## Declaration

### Syntax

```text
String(int kind, expr? len, string_length_kind len_kind,
    string_physical_type physical_type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the character kind. Kind 1, a byte, is the only one supported. |
| `len` | the length, as an expression, or `nil` when `len_kind` says there is none to give. |
| `len_kind` | how the length is determined; see [string_length_kind](../enum_nodes/string_length_kind.md). |
| `physical_type` | how the value is represented; see [StringPhysicalType](StringPhysicalType.md). |

### Return values

None. A type is not evaluated.

## Description

A Fortran character value is a sequence of bytes with a length, and the four
ways of fixing that length are what `len_kind` distinguishes: a constant or
runtime expression (`ExpressionLength`), inherited from the actual argument
(`AssumedLength`), decided at allocation (`DeferredLength`), or implied by the
operation that produced the value (`ImplicitLength`).

`physical_type` is separate from all of that: it says whether the value is
carried as a descriptor or as a bare pointer, and
[StringPhysicalCast](../expression_nodes/StringPhysicalCast.md) is what moves
between the two without changing the logical type.

## Examples

```clojure
(String
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
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/string_expr.asr
:language: clojure
```

## See Also

StringPhysicalType, [string_length_kind](../enum_nodes/string_length_kind.md), [StringConstant](../expression_nodes/StringConstant.md), [StringPhysicalCast](../expression_nodes/StringPhysicalCast.md)
