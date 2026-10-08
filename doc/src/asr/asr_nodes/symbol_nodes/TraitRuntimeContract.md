# TraitRuntimeContract

The canonical runtime callable layout of a nominal trait or anonymous conjunction.

## Declaration

### Syntax

```text
TraitRuntimeContract(symbol_table symtab, identifier name, symbol trait,
    trait_slot* slots, bool anonymous)
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

`anonymous` is explicit provenance, never inferred from a generated name.
For an anonymous conjunction, `trait` is a private, parent-only `Trait` whose
parents are original nominal requirements: duplicates and requirements already
implied by another parent are removed, then roots are sorted by their canonical
defining-scope identity. Singleton conjunctions reuse the original named
contract. Independent equivalent signatures retain all nominal origins.

Two anonymous contracts in independently compiled scopes are equal exactly when
their normalized original requirements agree. Their deterministic slot and
origin order therefore agrees too. A named child remains distinct from the
conjunction of its parents. Every allocation-slot intent and every defining
pointer-slot intent uses this equality, not directional weakening.

See [trait_slot](../helper_nodes/trait_slot.md) and
[TraitObjectType](../type_nodes/TraitObjectType.md).
