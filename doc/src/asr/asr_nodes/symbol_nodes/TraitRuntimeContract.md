# TraitRuntimeContract

The canonical runtime callable layout exported by a universal trait's module.

## Declaration

### Syntax

```text
TraitRuntimeContract(symbol_table symtab, identifier name, symbol trait,
    trait_slot* slots)
```

The symbol owns a scope of normalized interface functions. Each function takes
the same read-only trait view first, followed by the message's ordinary
arguments. This convention applies even to `nopass` implementations: their
selected adapter omits the receiver only in its final concrete call.

Slots follow the canonical trait hierarchy, coalescing compatible same-name
messages while retaining every nominal origin. Shared diamond origins occur
once. The verifier checks completeness, canonical order, signatures, and scope.
Contracts are currently materialized for the supported ordinary scalar method
subset; absence for other signatures means not implemented, not object-unsafe.

See [trait_slot](../helper_nodes/trait_slot.md) and
[TraitObjectType](../type_nodes/TraitObjectType.md).
