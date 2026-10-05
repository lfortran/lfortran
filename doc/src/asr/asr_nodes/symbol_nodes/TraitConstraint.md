# TraitConstraint

A trait requirement on a generic type parameter.

## Declaration

### Syntax

```text
TraitConstraint(symbol_table parent_symtab, identifier name, symbol parameter,
    symbol trait, trait_requirement* requirements,
    type_set_requirement* intrinsic_requirements)
```

### Arguments

| Argument | Description |
| --- | --- |
| `parent_symtab` | The generic scope containing this constraint. |
| `name` | Internal name of the constraint symbol. |
| `parameter` | Variable representing the deferred type parameter. |
| `trait` | Required nominal trait, using a visible import when necessary. |
| `requirements` | Mappings to normalized abstract procedures used by the generic body. |
| `intrinsic_requirements` | Complete all-member capability proofs for a finite type set; empty for universal traits. |

### Return values

None.

## Description

The generic body can use only its declared requirements. A normalized
procedure has an explicit first receiver argument of the deferred type.
Specialization substitutes a conformance witness for that procedure.
The mappings cover the trait's transitive requirements. Identical callable
contracts may share a normalized procedure, but their originating nominal
requirements are retained and all selected witnesses must agree.
References to signatures in a trait's nested scope use canonical
`ExternalSymbol` imports so serialization does not depend on symbol order.

Finite type sets instead use `intrinsic_requirements`, with no nominal member or
receiver. Each restriction has one concrete, one-operation witness for every
admitted member and no others. Operations are discovered while checking the
body; forwarding completes their closure before specialization copies bodies.
The requirements and witnesses live in the owning `Template` scope and are
serialized there. Concrete instantiation selects the witness matching the
actual category and kind through the shared template instantiator.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[Trait](Trait.md), [Template](Template.md),
[trait_requirement](../helper_nodes/trait_requirement.md),
[type_set_requirement](../helper_nodes/type_set_requirement.md)
