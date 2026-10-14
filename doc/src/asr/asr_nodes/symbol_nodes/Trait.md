# Trait

A nominal contract or an explicitly categorized finite intrinsic type set.

## Declaration

### Syntax

```text
Trait(symbol_table symtab, identifier name, symbol* parents, access access,
    trait_kind kind, ttype* member_types, trait_operation* operations)
```

### Arguments

| Argument | Description |
| --- | --- |
| `symtab` | Owned symbol table containing directly declared abstract message signatures. |
| `name` | The trait's name. |
| `parents` | Visible references to the parent traits whose requirements are inherited. |
| `access` | Visibility of the trait declaration. |
| `kind` | `UniversalTrait` or `IntrinsicTypeSet`. |
| `member_types` | Nonempty, unique, concrete scalar intrinsic types for a finite set; empty for universal traits. |
| `operations` | The directly declared members that are initializer or operator requirements, each with its operation; empty for type sets. |

### Return values

None.

## Description

A trait describes a contract, not an executable body or a derived-type layout.
A message can declare one receiver: a dummy declared
`type(itself), pass :: name`, of type `TypeParameter(itself)`, whose `Variable`
has `pass_attr` `Pass`. The receiver can have any position and name, and any
intent or none, with its ordinary Fortran meaning; nothing is inferred from a
name, a position or an unmarked `type(itself)` dummy. A message without one is
receiverless. Other `type(itself)` arguments and results of messages are not
implemented yet.
An ordinary message is a `Function`; a generic message is the existing
`Template` with its principal interface function and scoped `TraitConstraint`
binders. Equivalence compares binder positions and canonical nominal constraints,
not type-parameter spelling or currently visible concrete implementations.
The symbol identity distinguishes traits even when their signatures are equal.
The parent graph is acyclic. Inherited requirements retain the identity of
their original declaring trait rather than being copied into each child.
Shared ancestors contribute each canonical member once.

A universal trait can also require an initializer and intrinsic operators of
the implementing type. Such a member is an ordinary interface `Function` of
the trait's symbol table that has no receiver; its entry in `operations`
records what it requires, and the member's name has no semantic meaning. When
receivers or these signatures use the implementing type, the symbol table
also declares `itself`, a `Variable` of type `TypeParameter(itself)`; it is
not a member. An initializer returns `itself`; an operator
requirement has two operands, at least one of them `itself`. Two
requirements with the same operation and signature, `itself` standing for
one type, are one requirement of a composed constraint, and one trait does
not declare both.

An `IntrinsicTypeSet` has no nominal methods or parents in the current slice.
It admits exactly its listed intrinsic categories and kinds, and for a
character member its constant length, which distinguishes members as it
distinguishes character type arguments. A kind wildcard
of the source, such as `real(*)`, is already expanded into one member per
supported kind (`ASRUtils::supported_kinds`), so `member_types` lists only
concrete types. It is not an
ordinary union variable or runtime class, and cannot be manually implemented.
Its canonical symbol still distinguishes it from a separately declared trait
with identical member types.

An inline constraint is represented by a private `IntrinsicTypeSet` whose
parent scope is its defining `Template`. Exactly one `TraitConstraint` in
that scope references it. Template ownership is its provenance; the generated
symbol name has no semantic meaning. The identity and complete member list
survive generic specialization, serialization, and re-export. Module- and
program-owned named traits retain their existing representation.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](TraitConstraint.md), [TraitImplementation](TraitImplementation.md),
[trait_operation](../helper_nodes/trait_operation.md)
