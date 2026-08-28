# Enum

An enumeration definition.

## Declaration

### Syntax

```text
Enum(symbol_table symtab, identifier name, identifier* dependencies,
    identifier* members, abi abi, access access,
    enumtype enum_value_type, ttype type, symbol? parent)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the enumeration, holding one [Variable](Variable.md) per enumerator with its value in `symbolic_value` and `value`. |
| `name` | the name of the enumeration. |
| `dependencies` | the names of the symbols the definition refers to. |
| `members` | the names of the enumerators, in declaration order. |
| `abi` | `Source`, or `BindC` for `enum, bind(c)`. |
| `access` | `Public` or `Private`. |
| `enum_value_type` | how the enumerator values are distributed; see [enumtype](../enum_nodes/enumtype.md). It tells a backend whether the values can be treated as an index. |
| `type` | the integer type the enumerators are stored in. |
| `parent` | reserved for an extended enumeration; `nil` today. |

### Return values

None.

## Description

**Enum** defines a set of named integer constants. Every enumerator is a
[Variable](Variable.md) in `symtab` with `storage=Parameter` and a compile
time `value`, so an enumerator used in an expression needs nothing beyond the
ordinary constant folding.

`enum_value_type` records the shape of the value set:
`IntegerConsecutiveFromZero` allows a backend to use an enumerator directly as
a jump table index, while `NonInteger` forbids arithmetic on the values.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/enum.asr
:language: clojure
```

## See Also

[EnumType](../type_nodes/EnumType.md), [EnumConstructor](../expression_nodes/EnumConstructor.md), [EnumValue](../expression_nodes/EnumValue.md), [EnumName](../expression_nodes/EnumName.md), [Struct](Struct.md)
