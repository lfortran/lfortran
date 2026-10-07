# TraitProject

Weaken a runtime trait view to one of its declared nominal parents.

## Declaration

### Syntax

```text
TraitProject(expr view, int parent, ttype type)
```

`view` is a borrowed `TraitObjectType` or a nonowning
`Pointer(TraitObjectType)`. `parent` selects a direct parent of its canonical
contract's trait. `type` references that parent's runtime contract, preserving
the source's borrowed or pointer category.

The result retains the original payload and concrete metadata. Its witness is
the already-selected provider's parent table, not a new conformance lookup.
Transitive weakening is represented by successive direct-parent projections.
An unrelated trait or a stronger child cannot be selected by this operation.

A disassociated pointer produces a disassociated projection without
dereferencing its empty witness. The temporary header does not own the payload;
`TraitAssociate` copies it into independent persistent pointer storage when
needed. Provider-owned parent tables have static lifetime.
