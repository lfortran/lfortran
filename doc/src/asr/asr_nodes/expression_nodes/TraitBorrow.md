# TraitBorrow

Borrow scalar trait storage without copying or transferring ownership.

## Declaration

### Syntax

```text
TraitBorrow(expr owner, ttype type)
```

`owner` is a scalar variable or function reference of
`Allocatable(TraitObjectType(contract))`.
An allocatable component designator is also a variable: borrowing preserves
its containing object's lifetime and does not own or reassign that component.
It may also be a scalar variable of `Pointer(TraitObjectType(contract))`.
It may be an allocatable dummy slot, including INTENT(IN); borrowing neither
defines that slot nor changes which scope owns its allocation.
`type` is a bare view of that same canonical contract, visible in the borrowing
scope. The source's selected witness and concrete dynamic type remain unchanged,
regardless of other conformances visible at the borrowing site.

LLVM loads the allocated header from the owner's slot and rejects an
unallocated source. A pointer instead supplies its inline header; borrowing
checks that its payload is associated. The resulting view neither allocates payload storage nor
owns cleanup. It is suitable for a read-only trait dummy, dynamic call, or
checked owning copy. The verifier rejects using the view as an owner or
projecting it to another contract.

A function reference is first captured by `function_result_scope` into an
owning local of its using construct, then lowered through
`subroutine_from_function`. LLVM therefore borrows an ordinary slot; it does
not invent result ownership or return a borrowed header. The owned result is
finalized after the construct, not before the borrower returns.
