# TraitDictionaryValue

The dictionary of a concrete conformance passed to an erased binder.

## Declaration

### Syntax

```text
TraitDictionaryValue(symbol dictionary, ttype type)
```

`dictionary` is a visible `TraitDictionary` and `type` a
`TraitDictionaryType` of its contract. It is the trailing actual argument of a
call of a runtime generic message, an erased entry or a generic procedure
instantiated with an erased binder, for a binder whose type argument is
concrete. The caller never copies or creates the dictionary: it is
referenced from the conformance's module.

At a call boundary, an erased dummy of the binder takes the type that the
dictionary's implementation describes (see
[TraitErasedType](../type_nodes/TraitErasedType.md)).
