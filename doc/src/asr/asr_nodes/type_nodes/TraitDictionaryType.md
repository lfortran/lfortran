# TraitDictionaryType

A conformance dictionary laid out by a dictionary contract.

## Declaration

### Syntax

```text
TraitDictionaryType(symbol contract, identifier? parameter)
```

`contract` is a visible `TraitDictionaryContract`. A dictionary dummy names
the erased `parameter` it describes; the type of a dictionary actual has none.
Dictionaries are only passed, never stored, assigned or compared; two
dictionary types are equal when their contracts are. In LLVM a dictionary is
the address of its table.
