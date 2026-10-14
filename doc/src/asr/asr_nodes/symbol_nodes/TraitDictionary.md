# TraitDictionary

The dictionary of one conformance to a protocol trait.

## Declaration

### Syntax

```text
TraitDictionary(symbol_table symtab, identifier name, symbol contract,
    symbol implementation, symbol* procedures, identifier* dependencies, abi abi)
```

`contract` is the `TraitDictionaryContract` that lays the dictionary out and
`implementation` the `TraitImplementation` it describes. The module that
declares the conformance creates the dictionary with the conformance, for
each protocol trait of its hierarchy, so the evidence lives with its owner:
a client only references it, and a provider compiled before the conformance
existed reaches it through its erased entries without recompilation.

`procedures` are the adapters of the contract's slots, in slot order, owned by
the dictionary's scope. Each has the slot's erased interface; it recovers the
concrete values of its erased arguments, receiver and result with
[TraitRecover](../expression_nodes/TraitRecover.md) and calls the conformance's
binding, dropping the receiver of a `nopass` binding. `dependencies` names the
adapters, and `abi` is `ExternalUndefined` once the module is loaded.

Values are copied bitwise, so only conformances whose type is a plain
intrinsic numeric or logical type, or a derived type of such components and
fixed-size arrays without finalization, have a dictionary. An adapter modifies
its erased receiver in place, through `TraitRecover`, exactly when its slot
takes the receiver `intent(inout)`. A conformance outside a module
has none either. Using such a conformance at runtime is diagnosed with the
reason at the call.

In LLVM the dictionary is the constant table `__trait_dictionary_<name>`
defined by its module: the size of a value, a procedure that default
initializes the storage of a derived type value (null otherwise), then the
adapters in slot order. A `TraitDictionaryType` value is the address of that
table. The table describes no copy or destruction procedure, since values
are copied bitwise and own nothing.

Verification checks the contract and implementation, the adapters' slot
signatures and owner, and that `TraitRecover` names the dictionary.
