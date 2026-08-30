# ttype

The types of ASR.

## Declaration

### Syntax

```text
ttype
    = Integer(int kind)
    | UnsignedInteger(int kind)
    | Real(int kind)
    | Complex(int kind)
    | String(int kind, expr? len, string_length_kind len_kind, string_physical_type physical_type)
    | Logical(int kind)
    | Set(ttype type)
    | List(ttype type)
    | Tuple(ttype* type)
    | StructType(ttype* data_member_types, ttype* member_function_types, bool is_cstruct, bool is_unlimited_polymorphic)
    | EnumType(symbol enum_type)
    | UnionType(ttype* data_member_types)
    | Dict(ttype key_type, ttype value_type)
    | Pointer(ttype type)
    | Allocatable(ttype type)
    | CPtr()
    | SymbolicExpression()
    | TypeParameter(identifier param)
    | Array(ttype type, dimension* dims, array_physical_type physical_type)
    | FunctionType(ttype* arg_types, ttype? return_var_type, abi abi, deftype deftype, string? bindc_name, bool elemental, bool pure, bool module, bool inline, bool static, symbol* restrictions, bool is_restriction, bool external_abi)
```

### Arguments

None.

### Return values

None. A type is not evaluated.

## Description

A `ttype` describes what a value is: the type of every expression, of every
variable and of every procedure argument is one of these.

The types divide into a few groups:

- the intrinsic scalar types, [Integer](Integer.md),
  [UnsignedInteger](UnsignedInteger.md), [Real](Real.md),
  [Complex](Complex.md), [Logical](Logical.md) and [String](String.md), each
  fixed by a [kind](../kinds_nodes/kinds.md);
- the derived types, [StructType](StructType.md), [EnumType](EnumType.md) and
  [UnionType](UnionType.md), whose definitions are symbols;
- the composite types [Array](Array.md), [Pointer](Pointer.md) and
  [Allocatable](Allocatable.md), which wrap another type;
- [CPtr](CPtr.md) and [FunctionType](FunctionType.md), which describe a C
  address and a procedure signature;
- [TypeParameter](TypeParameter.md), a placeholder inside a generic
  [Template](../symbol_nodes/Template.md);
- the LPython container types [List](List.md), [Set](Set.md), [Dict](Dict.md)
  and [Tuple](Tuple.md), and [SymbolicExpression](SymbolicExpression.md).

A type separates what a value *is* from how it is *represented*. An
[Array](Array.md) carries an `array_physical_type` and a
[String](String.md) a `string_physical_type`, and two values with the same
logical type may differ in it; the physical casts,
[ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md) and
[StringPhysicalCast](../expression_nodes/StringPhysicalCast.md), convert
between representations without changing the value, while
[Cast](../expression_nodes/Cast.md) changes the value itself.

## See Also

[kinds](../kinds_nodes/kinds.md), [Cast](../expression_nodes/Cast.md),
[symbol](../symbol_nodes/symbol.md)

## Type Nodes

```{toctree}
---
maxdepth: 1
---
Allocatable
Array
Complex
CPtr
Dict
EnumType
FunctionType
Integer
List
Logical
Pointer
Real
Set
String
StringPhysicalType
StructType
SymbolicExpression
Tuple
TypeParameter
UnionType
UnsignedInteger
```
