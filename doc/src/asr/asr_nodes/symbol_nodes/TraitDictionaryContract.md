# TraitDictionaryContract

The layout of the conformance dictionaries of a protocol trait.

## Declaration

### Syntax

```text
TraitDictionaryContract(symbol_table symtab, identifier name, symbol trait,
    trait_slot* slots)
```

A protocol trait is a universal trait that requires an initializer
(`initial`) or an operator of `type(itself)`. Such a trait is
not a runtime view: `class(trait)` is diagnosed, and the symbol table visitor
creates this contract next to the trait instead of a `TraitRuntimeContract`.
A binder of a runtime generic message constrained by exactly one protocol
trait is erased (see [TraitErasedType](../type_nodes/TraitErasedType.md)) and
each call passes the dictionary of the conformance selected for it.

The contract owns one interface function per slot, in the canonical order of
the trait hierarchy. Messages coalesce by name, and equal operations whose
signatures agree share one slot, like the requirements of a constraint; every
slot retains its nominal `origins`. Each interface takes the dictionary first
(`TraitDictionaryType(contract, itself)`), then the erased receiver of a
message, moved first from its source position or added for a receiverless
message with the intent of a runtime slot's view (see
[TraitRuntimeContract](TraitRuntimeContract.md)), then the member's other
arguments with `type(itself)` replaced by `TraitErasedType(itself)`. An `itself` result is erased the same way.

A dictionary laid out by this contract also records the size and the default
initializer of its values. The LLVM table is `[size, initializer or null,
one adapter per slot]`; it is compiler-private, not a public ABI.

See [TraitDictionary](TraitDictionary.md), [trait_slot](../helper_nodes/trait_slot.md)
and the [traits guide](../../traits.md).
