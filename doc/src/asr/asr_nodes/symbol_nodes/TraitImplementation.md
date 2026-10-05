# TraitImplementation

A concrete type's nominal conformance to a trait.

## Declaration

### Syntax

```text
TraitImplementation(symbol_table parent_symtab, identifier name,
    ttype implementing_type, symbol? type_declaration, symbol trait,
    trait_binding* bindings, access access)
```

### Arguments

| Argument | Description |
| --- | --- |
| `parent_symtab` | Scope defining the implementation. |
| `name` | Internal name of the conformance record. |
| `implementing_type` | Concrete structural type information. |
| `type_declaration` | Nominal derived-type symbol; required for a derived-type conformance. |
| `trait` | Trait being implemented. |
| `bindings` | One compatible implementation for each canonical message in the trait's transitive requirements. |
| `access` | Visibility of the conformance evidence. |

### Return values

None.

## Description

Conformance belongs to its defining scope and does not mutate an imported
derived type. Structural layout equality is insufficient for selecting it:
`type_declaration` preserves nominal type identity.
Bindings preserve canonical member identities and receiver adaptation.
An implementation also satisfies the trait's ancestors. When several visible
paths satisfy the same constraint, their procedure and receiver bindings must
agree; distinct nominal conformance paths are not resolved by import order.
This record itself generates no runtime code in the static-traits subset.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[Trait](Trait.md), [trait_binding](../helper_nodes/trait_binding.md)
