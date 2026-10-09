# TraitImplementation

A resolved type's complete nominal conformance to a trait.

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
| `implementing_type` | Resolved storage type information, not a generic type parameter. |
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
A generic member and its implementation reference `Template` symbols.
Their scoped binders correspond positionally and must have equivalent nominal
constraints; an implementation cannot narrow the universally promised domain.
An implementation also satisfies the trait's ancestors. When several visible
paths satisfy the same constraint, their procedure and receiver bindings must
agree; distinct nominal conformance paths are not resolved by import order.
This record itself generates no runtime code in the static-traits subset.

Derived-type adoption uses the same complete records as retroactive blocks.
`Struct::trait_obligations` separately retains the nominal requirements,
including inherited ones. Abstract types can defer unimplemented requirements;
they never get a partial `TraitImplementation`. For declaration adoption,
bindings must agree with the effective ordinary type-bound procedures, including
overrides. An inherited receiver can be a polymorphic nominal ancestor while
`type_declaration` continues to identify the implementing descendant. Runtime
witnesses are generated only for concrete, eligible types and interfaces.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[Trait](Trait.md), [trait_binding](../helper_nodes/trait_binding.md)
