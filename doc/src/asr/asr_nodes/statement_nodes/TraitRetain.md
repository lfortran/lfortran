# TraitRetain

Transfer a consumed owning temporary into its using scope's retained-result
storage.

## Declaration

### Syntax

```text
TraitRetain(expr storage, expr owner)
```

Both operands are local variables in the same scope. `storage` has type
`TraitOwnerList`; `owner` is a non-target scalar
`Allocatable(TraitObjectType)` of the same canonical contract.

The operation is emitted after the statement that consumes a borrowed view of
the result. It moves the owned header into the store and resets the source slot
to unallocated. An empty source is a no-op. No payload copy, witness reselection,
defined assignment, or user finalization occurs.

Repeated evaluation of a scalar factory can therefore reuse its result slot
without destroying previous results prematurely. Store cleanup, not the
per-iteration consuming statement, runs the retained values' finalizers when the
original using construct completes.

The node is compiler-only and has unchecked dynamic lifecycle effects. It is
not a general source-language MOVE_ALLOC or a borrowed pointer association.
