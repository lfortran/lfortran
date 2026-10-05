# TraitConstraint

A trait requirement on a generic type parameter.

## Declaration

### Syntax

```text
TraitConstraint(symbol_table parent_symtab, identifier name, symbol parameter,
    symbol trait, trait_requirement* requirements)
```

### Arguments

| Argument | Description |
| --- | --- |
| `parent_symtab` | The generic scope containing this constraint. |
| `name` | Internal name of the constraint symbol. |
| `parameter` | Variable representing the deferred type parameter. |
| `trait` | Required nominal trait, using a visible import when necessary. |
| `requirements` | Mappings to normalized abstract procedures used by the generic body. |

### Return values

None.

## Description

The generic body can use only its declared requirements. A normalized
procedure has an explicit first receiver argument of the deferred type.
Specialization substitutes a conformance witness for that procedure.
References to signatures in a trait's nested scope use canonical
`ExternalSymbol` imports so serialization does not depend on symbol order.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[Trait](Trait.md), [Template](Template.md), [trait_requirement](../helper_nodes/trait_requirement.md)
