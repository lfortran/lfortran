# Trait

A nominal collection of receiver-independent procedure signatures.

## Declaration

### Syntax

```text
Trait(symbol_table symtab, identifier name, symbol* parents, access access)
```

### Arguments

| Argument | Description |
| --- | --- |
| `symtab` | Owned symbol table containing directly declared abstract message signatures. |
| `name` | The trait's name. |
| `parents` | Visible references to the parent traits whose requirements are inherited. |
| `access` | Visibility of the trait declaration. |

### Return values

None.

## Description

A trait describes a contract, not an executable body or a derived-type layout.
Its procedures have no passed-object dummy, but can have ordinary arguments.
The symbol identity distinguishes traits even when their signatures are equal.
The parent graph is acyclic. Inherited requirements retain the identity of
their original declaring trait rather than being copied into each child.
Shared ancestors contribute each canonical member once.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](TraitConstraint.md), [TraitImplementation](TraitImplementation.md)
