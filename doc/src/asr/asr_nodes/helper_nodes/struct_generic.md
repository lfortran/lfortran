# struct_generic

What makes a [Struct](../symbol_nodes/Struct.md) the definition of a generic
derived type, or an instance of one.

## Declaration

### Syntax

```text
struct_generic
    = GenericTypeDefinition(symbol* parameters)
    | GenericTypeInstance(symbol definition, generic_type_argument* arguments)
```

### Arguments

| Argument | Description |
| --- | --- |
| `parameters` | The type parameters of a definition: the binder `Variable`s of its package, in the package's argument order. |
| `definition` | The definition `Struct` an instance instantiates, through an import when the instance is outside the package. |
| `arguments` | One [generic_type_argument](generic_type_argument.md) per parameter of the definition, in order. |

### Return values

None.

## Description

A generic derived type is a `Template` package owned by a module. The symbol
of the package's name is the definition `Struct`, which carries
`GenericTypeDefinition`; the package also owns the binders, their
`TraitConstraint`s, the procedures that depend on the parameters (a method
with its own binders is a nested `Template`), the initializer generic and the
conformances the definition adopts. Code is never generated for a package.

An instance is an ordinary `Struct` of the module or program that uses the
type with concrete type arguments; it carries `GenericTypeInstance`. Its
components, obligations and bindings are those of the definition, and its
procedures and conformances are copies from the package owned by the same
module or program. A module or program has at most one instance for equal
type arguments with agreeing evidence.

Verification checks that a definition is the symbol of its package's name in
a module and records the package's binders, that an instance names such a
definition with one concrete argument per parameter and the evidence of each
universal-trait constraint, that it has the definition's components,
obligations and bindings, that no two instances of one scope are equal, and
that neither a definition nor an instance extends or is extended by a type.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[generic_type_argument](generic_type_argument.md),
[Struct](../symbol_nodes/Struct.md),
[Template](../symbol_nodes/Template.md)
