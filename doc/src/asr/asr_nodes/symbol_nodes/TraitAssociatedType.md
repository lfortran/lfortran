# TraitAssociatedType

A scoped mapping from a constrained binder's associated type to a private
deferred type parameter.

## Declaration

### Syntax

```text
TraitAssociatedType(symbol_table parent_symtab, identifier name,
    symbol parameter, symbol associated_type, symbol type_parameter)
```

### Arguments

| Argument | Description |
| --- | --- |
| `parent_symtab` | Generic scope containing the mapping, binder and placeholder. |
| `name` | Internal name of this mapping in that scope. |
| `parameter` | Deferred type declaration for the constrained binder. |
| `associated_type` | Associated type declaration in the required trait hierarchy, through a visible import when necessary. |
| `type_parameter` | Fresh private deferred type declaration representing this binder's associated type. |

### Return values

None.

## Description

Each constrained binder has exactly one mapping for each associated declaration
of its trait hierarchy. The binder and placeholder are distinct `Variable`
symbols of `TypeParameter` type in `parent_symtab`; no two mappings share the
placeholder. The associated declaration belongs to a required nominal trait
and cannot be its `itself` receiver.

Normalized generic requirement signatures use the placeholder unless an
explicit associated-type equality supplies its effective type. Conformance
selection substitutes the type inferred from the checked implementation, or
the caller's associated placeholder when forwarding. This is compile-time
metadata, not an additional generic argument or a runtime object.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](TraitConstraint.md), [Trait](Trait.md),
[trait_same_type_requirement](../helper_nodes/trait_same_type_requirement.md)
