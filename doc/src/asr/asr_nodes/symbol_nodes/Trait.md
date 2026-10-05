# Trait

A nominal contract or an explicitly categorized finite intrinsic type set.

## Declaration

### Syntax

```text
Trait(symbol_table symtab, identifier name, symbol* parents, access access,
    trait_kind kind, ttype* member_types)
```

### Arguments

| Argument | Description |
| --- | --- |
| `symtab` | Owned symbol table containing directly declared abstract message signatures. |
| `name` | The trait's name. |
| `parents` | Visible references to the parent traits whose requirements are inherited. |
| `access` | Visibility of the trait declaration. |
| `kind` | `UniversalTrait` or `IntrinsicTypeSet`. |
| `member_types` | Nonempty, unique, concrete scalar numeric types for a finite set; empty for universal traits. |

### Return values

None.

## Description

A trait describes a contract, not an executable body or a derived-type layout.
Its procedures have no passed-object dummy, but can have ordinary arguments.
The symbol identity distinguishes traits even when their signatures are equal.
The parent graph is acyclic. Inherited requirements retain the identity of
their original declaring trait rather than being copied into each child.
Shared ancestors contribute each canonical member once.

An `IntrinsicTypeSet` has no nominal methods or parents in the current slice.
It admits exactly its listed intrinsic categories and kinds. It is not an
ordinary union variable or runtime class, and cannot be manually implemented.
Its canonical symbol still distinguishes it from a separately declared trait
with identical member types.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](TraitConstraint.md), [TraitImplementation](TraitImplementation.md)
