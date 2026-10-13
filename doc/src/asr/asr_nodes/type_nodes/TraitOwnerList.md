# TraitOwnerList

Compiler-owned storage retaining scalar trait function results until their
original using construct completes.

## Declaration

### Syntax

```text
TraitOwnerList(symbol contract)
```

`contract` is a visible reference to the canonical `TraitRuntimeContract`.
The store is an initially empty, noncopyable local of a BLOCK or ASSOCIATE
scope. It is not a source-language list, array, trait-object component,
procedure argument, or function result.

Each retained value keeps its independently allocated header, payload, selected
witness and concrete lifecycle. Different dynamic implementations may coexist.
`TraitRetain` transfers an owning temporary into the store after its consumer
has finished, without copying or invoking FINAL.

Scope completion destroys every retained value once and releases all store
nodes. Normal completion and RETURN, EXIT, CYCLE or GO TO use the same existing
scope cleanup. Re-entry starts with an empty store. The compiler-private LLVM
representation is an owning chain of headers; each chain is released in reverse
retention order. No ordering between independent stores is promised.
