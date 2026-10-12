# TraitDeferredDictionary

The dictionary of a binder whose type is still deferred.

## Declaration

### Syntax

```text
TraitDeferredDictionary(symbol constraint, ttype type)
```

`constraint` is the visible `TraitConstraint` of a scoped binder of a checked
generic definition, and `type` the `TraitDictionaryType` the call requires.
It exists only inside a checked generic definition, like
[TraitDeferredPack](TraitDeferredPack.md). The template instantiator replaces
it with:

- a [TraitDictionaryValue](TraitDictionaryValue.md) of the conformance
  selected for a concrete type argument where the specialization was checked;
- another `TraitDeferredDictionary` of the forwarding caller's binder; or
- the dictionary dummy of an erased procedure for an erased binder.

Code generation rejects a deferred dictionary that has not been instantiated.
