# TraitObjectType

A runtime view of a nominal universal trait, distinct from concrete storage and
ordinary implementation inheritance.

## Declaration

### Syntax

```text
TraitObjectType(symbol contract)
```

`contract` references the canonical `TraitRuntimeContract`, through a visible
symbol. Its slot interfaces describe all ordinary arguments and results.
A bare view is a required, nonpointer, nonallocatable scalar `intent(in)` dummy.
`Allocatable(TraitObjectType)` instead denotes scalar owning storage: a local,
saved, module or BLOCK variable, or an allocatable dummy's caller-owned slot.
Every dummy intent requires the same canonical declared contract and an
allocatable actual; child contracts and concrete allocatables are not covariant
slots. INTENT(IN) permits inquiry and borrowing but cannot define the slot.
INTENT(OUT) entry cleanup uses the ordinary `intent_out_deallocate` pass.
Dummies are never destroyed at callee scope exit. Optional and BIND(C) slots,
unproved PURE output cleanup, and erased function results remain unsupported.
`TraitPack` creates compiler-borrowed concrete views; forwarding
uses the original variable. `TraitBorrow` borrows an allocated owner. None of
these operations copies the payload or transfers ownership.

Owning allocation and assignment use `TraitAllocate` and `TraitAssignment`;
ordinary deallocation statements release only verified owners. The verifier
rejects ordinary header assignment, association and nullification that would
duplicate or discard ownership.

The LLVM representation carries concrete CLASS metadata, a payload address, and
an independent selected witness. It is compiler-private, not a public ABI.
Type-set traits cannot form views. Generic-method universal traits remain a
future runtime implementation stage, not a permanently excluded trait category.

See the [traits guide](../../traits.md).
