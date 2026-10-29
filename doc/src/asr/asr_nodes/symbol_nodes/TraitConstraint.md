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
| `requirements` | Mappings to normalized abstract procedures used by the generic body; for a generic message, a generic requirement `Template` followed by its published specializations. |
| `intrinsic_requirements` | Complete all-member capability proofs for a finite type set; empty for universal traits. |

### Return values

None.

## Description

The generic body can use only its declared requirements. A normalized
procedure has a first receiver argument of the deferred type: the message's
receiver, moved there from any position with its intent and `Pass` marker, or
for a receiverless message a new `intent(in)` dummy without the marker that
only carries the dispatch. The other arguments keep their source order.
Specialization substitutes a conformance witness for that procedure.
The mappings cover the trait's transitive requirements. Identical callable
contracts may share a normalized procedure, but their originating nominal
requirements are retained and all selected witnesses must agree.
References to signatures in a trait's nested scope use canonical
`ExternalSymbol` imports so serialization does not depend on symbol order.

For a trait with associated types, the same scope also contains one
`TraitAssociatedType(parent_symtab, name, parameter, associated_type,
type_parameter)` mapping per associated declaration and constrained binder.
`associated_type` references the declaration in the trait; `type_parameter`
is a fresh private `Variable` of `TypeParameter` type. Every normalized
requirement uses that placeholder for the associated declaration. These are
dependent parameters, not additional arguments of the enclosing `Template`.
Conformance selection substitutes the types inferred from its checked
bindings, or the corresponding placeholders of a forwarding caller.
The verifier requires complete, unique mappings and matching signatures.

A generic message instead maps to a generic requirement: a `Template` in the
same scope whose fresh binders keep the message's nominally equivalent
constraints and whose function puts the receiver first in the same way. Each tuple of type
arguments the body uses adds one more requirement for the same member: a
bodyless specialization of that function, recording its type arguments in its
copies of the binder declarations. Specialization binds the generic
requirement once and maps each published specialization to a static adapter of
the selected generic implementation, or to the forwarding caller's same
specialization. A specialization with concrete type arguments also records the
[evidence](../helper_nodes/trait_evidence.md) selected where the body made the
call, one conformance per nominal constraint of the generic requirement, and
its adapters bind the implementation's constraints from it. Equal type
arguments with different evidence are distinct specializations. A constraint
is serialized after the other symbols of its scope, since its evidence refers
into their symbol tables.

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
