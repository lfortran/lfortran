# Struct

A derived type definition.

## Declaration

### Syntax

```text
Struct(symbol_table symtab, identifier name, ttype struct_signature,
    identifier* dependencies, identifier* members,
    identifier* member_functions, abi abi, access access,
    bool is_packed, bool is_abstract, bool is_sequence,
    call_arg* initializers, expr? alignment, symbol? parent,
    identifier* kind_params)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the type: one [Variable](Variable.md) per data component and one [StructMethodDeclaration](StructMethodDeclaration.md) per type-bound procedure. |
| `name` | the name of the derived type. |
| `struct_signature` | the [StructType](../type_nodes/StructType.md) that describes an instance of this type. |
| `dependencies` | the names of the symbols the definition refers to. |
| `members` | the names of the data components, in declaration order. The order fixes the storage layout. |
| `member_functions` | the names of the type-bound procedures. |
| `abi` | `Source` for a type defined here, `BindC` for `bind(c)`. |
| `access` | `Public` or `Private`. |
| `is_packed` | `true` when the layout must not be padded. |
| `is_abstract` | `true` for an `abstract` type, which cannot be instantiated. |
| `is_sequence` | `true` for a `sequence` type, whose components are laid out in declaration order with no padding. |
| `initializers` | the default initializers of the components, in `members` order. |
| `alignment` | an explicit alignment in bytes, or `nil`. |
| `parent` | the type this one extends, or `nil`. |
| `kind_params` | the names of the kind type parameters of the type. |

### Return values

None.

## Description

**Struct** is the definition of a derived type, and it is a symbol: it is
owned by a symbol table and referred to by name. The type of a *variable* of
that type is the separate [StructType](../type_nodes/StructType.md), which
holds the component types.

`members` is authoritative for layout. The symbol table is a mapping and says
nothing about order, so a backend that walks components must walk `members` and
look each name up, never iterate the symbol table.

A component is read with
[StructInstanceMember](../expression_nodes/StructInstanceMember.md) and a whole
value is built with
[StructConstructor](../expression_nodes/StructConstructor.md).

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/struct.asr
:language: clojure
```

## See Also

[StructType](../type_nodes/StructType.md), [StructInstanceMember](../expression_nodes/StructInstanceMember.md), [StructConstructor](../expression_nodes/StructConstructor.md), [StructMethodDeclaration](StructMethodDeclaration.md), [Union](Union.md)
