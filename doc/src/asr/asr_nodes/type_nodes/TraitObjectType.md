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
Dummies are never destroyed at callee scope exit. A scalar allocatable function
result uses `ReturnVar` before normal result lowering, then a hidden OUT slot.
The caller owns its returned value until its innermost using construct completes.
Importing an explicit function interface as `ExternalUndefined` preserves this
Fortran calling convention and ownership; the provider's concrete declarations
are not needed in the caller.
Optional and BIND(C) slots, and unproved PURE output/result cleanup, remain
unsupported.
`Pointer(TraitObjectType)` denotes a persistent nonowning view, with an
independent association descriptor. `TraitAssociate` copies an existing target
view into that descriptor; NULLIFY resets it. Pointer IN protects association,
not the target. Defining pointer dummies require a pointer actual; pointer IN
can also receive an eligible concrete TARGET or allocatable TARGET owner.
As in ordinary Fortran, a PURE procedure cannot have a polymorphic INTENT(OUT)
dummy, even when it is a pointer.
`TraitPack` creates compiler-borrowed concrete views; forwarding
uses the original variable. `TraitBorrow` borrows an allocated owner or an
associated pointer. None of
these operations copies the payload or transfers ownership.

Owning allocation and assignment use `TraitAllocate` and `TraitAssignment`;
ordinary deallocation statements release only verified owners. The verifier
rejects ordinary header assignment and association that would duplicate or
discard ownership. NULLIFY is allowed only for definable pointer descriptors.

The LLVM representation carries a three-word common prefix (concrete CLASS
metadata, payload address, concrete lifecycle) followed by one inline selected
method address per declared contract slot. It is compiler-private, not a public ABI.
Pointer descriptors are inline headers passed by address, whereas owning
allocation slots hold independently allocated headers. Pointer descriptor
cleanup never destroys the target. All method storage belongs to the descriptor;
association and projection cannot leave pointers into another view's stack frame.
Type-set traits cannot form views. Generic-method universal traits remain a
future runtime implementation stage, not a permanently excluded trait category.

See the [traits guide](../../traits.md).
