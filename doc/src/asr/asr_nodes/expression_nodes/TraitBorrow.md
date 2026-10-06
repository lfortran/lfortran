# TraitBorrow

Borrow an allocated scalar trait owner without copying or transferring ownership.

## Declaration

### Syntax

```text
TraitBorrow(expr owner, ttype type)
```

`owner` is a scalar variable of `Allocatable(TraitObjectType(contract))`.
`type` is a bare view of that same canonical contract, visible in the borrowing
scope. The source's selected witness and concrete dynamic type remain unchanged,
regardless of other conformances visible at the borrowing site.

LLVM loads the allocated header from the owner's slot and rejects an
unallocated source. The resulting view neither allocates payload storage nor
owns cleanup. It is suitable for a read-only trait dummy, dynamic call, or
checked owning copy. The verifier rejects using the view as an owner or
projecting it to another contract.
