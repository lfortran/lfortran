# trait_lifecycle

The concrete nominal identity of the scalar owned-value protocol.

## Declaration

### Syntax

```text
trait_lifecycle = (symbol type_declaration)
```

This record belongs to a `TraitWitness` but references the canonical concrete
type, not a trait or conformance identity. Verification requires agreement with
the witness's `TraitImplementation`. Abstract compile-only evidence is allowed;
owning construction additionally requires an instantiable type.

Private concrete-owned helpers default-initialize raw storage, initialize-copy
into fresh storage without defined assignment or finalization, assign into
prepared/live storage, destroy a live raw value, and release a compiler's
snapshot without user finalization. Component
allocation/deepcopy, pointer association, default initialization and finalization
use the existing concrete machinery. A fresh destination is explicitly
distinguished from live assignment, including nested dynamic components.
Component-defined assignment acts on the actual LHS, not a default-initialized
snapshot. SOURCE and snapshot capture copy values without invoking those
assignment procedures.

The runtime descriptor has canonical concrete-type linkage. Multiple selected
witnesses can reference it without changing dynamic type identity or multiplying
finalization. It does not allocate or destroy an ordinary CLASS wrapper as if
that wrapper were a larger trait header.
