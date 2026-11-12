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
    identifier* kind_params, symbol? intent_out_reset)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the type: one [Variable](Variable.md) per data component and one [StructMethodDeclaration](StructMethodDeclaration.md) per type-bound procedure. |
| `name` | the name of the derived type. |
| `struct_signature` | the [StructType](../type_nodes/StructType.md) that describes an instance of this type, with `is_cstruct` true (`asr.verify.struct.signature_is_instance_type`). |
| `dependencies` | the names of the symbols the definition refers to. |
| `members` | the names of the data components, in declaration order. The order fixes the storage layout. |
| `member_functions` | the names of the FINAL subroutines in the defining scope. User type-bound procedures are `StructMethodDeclaration` symbols in `symtab`. |
| `abi` | `Source` for a type defined here, `BindC` for `bind(c)`. |
| `access` | `Public` or `Private`. |
| `is_packed` | `true` when the layout must not be padded. |
| `is_abstract` | `true` for an `abstract` type, which cannot be instantiated. |
| `is_sequence` | `true` for a `sequence` type, whose components are laid out in declaration order with no padding. |
| `initializers` | the default initializers of the components, in `members` order. |
| `alignment` | an explicit alignment in bytes, or `nil`. |
| `parent` | the type this one extends, or `nil`. |
| `kind_params` | the names of the kind type parameters of the type. |
| `intent_out_reset` | the compiler-generated reset subroutine in the type's nearest enclosing procedure-owning scope, or `nil` before synthesis or for a nonconcrete type. This is not a user-visible type-bound procedure. |

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

Uninstantiated parameterized types, including extensions that only inherit
their parameters, have no reset helper. A monomorphized concrete type has its
own helper; its complete parent chain must have concrete parameters.

The frontend reserves `intent_out_reset` before saving module files; the
`intent_out_deallocate` pass fills its body using the same cleanup and
default-initialization emitters as nonpolymorphic entry handling. The
canonical symbol reference, rather than a helper-name convention, connects
the type and its helper across imports and separate compilation. The
receiver must be a required scalar `type(T), intent(inout)` argument of
exactly this defining type; the ASR verifier checks this invariant.
For a type declared in a BLOCK or ASSOCIATE construct, the helper belongs to
the enclosing procedure or program, because constructs cannot own procedure
implementations. The type stays in its original scope; an `ExternalSymbol`
in the helper names it through the visible outer construct and its nested
scope path. Module, program and procedure types keep helpers in their own
defining scopes. No backend-specific traversal of block implementations is
needed.
See [IntentOutReset](../statement_nodes/IntentOutReset.md).

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/struct.asr
:language: clojure
```

## See Also

[StructType](../type_nodes/StructType.md), [StructInstanceMember](../expression_nodes/StructInstanceMember.md), [StructConstructor](../expression_nodes/StructConstructor.md), [StructMethodDeclaration](StructMethodDeclaration.md), [Union](Union.md)
