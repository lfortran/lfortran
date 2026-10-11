# TraitProject

Weaken a runtime trait view to guaranteed nominal requirements.

## Declaration

### Syntax

```text
TraitProject(expr view, trait_projection_slot* slots, ttype type)
```

`view` is a borrowed `TraitObjectType` or a nonowning
`Pointer(TraitObjectType)`. `type` references a named parent or anonymous subset
contract, preserving the source's borrowed or pointer category. `slots` records,
in target layout order, the source slot for each target callable.

The verifier first proves the target's original nominal requirements from the
source's ancestry, then checks that every slot preserves all target origins and
its ordinary signature. It does not accept matching signatures as nominal
evidence. An unrelated trait or a stronger named child cannot be selected.

The result copies the original payload address, concrete metadata, lifecycle
and selected method addresses into its own descriptor. A target conjunction may
be declared only in a late independently compiled client; the provider need not
pre-emit it or expose its source. LLVM follows the verified slot map mechanically.

A disassociated pointer produces a disassociated projection without reading
method slots. The temporary descriptor does not own the payload;
`TraitAssociate` copies all its inline slots into independent persistent pointer
storage. No method-table pointer refers to a callee's stack, and projection needs
no heap allocation, cache, reference counting or conformance reselection.
