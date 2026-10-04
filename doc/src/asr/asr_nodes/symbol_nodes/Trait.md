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
| `symtab` | Owned symbol table containing the abstract message signatures. |
| `name` | The trait's name. |
| `parents` | Parent traits; empty in the initial static-traits implementation. |
| `access` | Visibility of the trait declaration. |

### Return values

None.

## Description

A trait describes a contract, not an executable body or a derived-type layout.
Its procedures have no passed-object dummy, but can have ordinary arguments.
The symbol identity distinguishes traits even when their signatures are equal.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](TraitConstraint.md), [TraitImplementation](TraitImplementation.md)
