# TraitDeferredPack

Checked nominal evidence for a readonly argument whose type is still deferred.

## Declaration

### Syntax

```text
TraitDeferredPack(expr payload, symbol constraint, ttype type)
```

`payload` is a scalar `TypeParameter` expression. `constraint` is a visible
`TraitConstraint` of that expression's scoped binder, and `type` is the required
borrowed `TraitObjectType`. The declared constraint must nominally imply the
requested contract. Structural method similarity is not conformance.

This expression exists only inside a checked generic definition. The existing
template instantiator supplies explicit evidence and replaces it with:

- `TraitPack` for a concrete actual and its selected witness;
- another `TraitDeferredPack` referring to the forwarding caller's binder; or
- `TraitProject` with a verified slot map for an already-erased argument.

Neither the original nor the substituted node copies or owns payload storage.
Code generation rejects a deferred pack that has not been instantiated.
