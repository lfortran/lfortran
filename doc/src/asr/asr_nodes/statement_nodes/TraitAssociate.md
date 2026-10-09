# TraitAssociate

Associate a persistent, nonowning scalar trait pointer with a target, or reset
its association.

## Declaration

### Syntax

```text
TraitAssociate(expr target, expr? value)
```

`target` is a definable variable of `Pointer(TraitObjectType)`. `value` is a
pointer of the same canonical contract or an explicit borrowed view of existing
TARGET/POINTER storage. An absent value disassociates the pointer.

Association copies the concrete metadata, payload address and already-selected
witness into the destination pointer's own descriptor. It does not copy the
payload, reselect conformance, allocate an owner, or invoke FINAL. Reassociating
or nullifying one pointer therefore leaves independently associated aliases
unchanged. The usual Fortran target-lifetime and PURE restrictions still apply.

The LLVM pointer descriptor has a common three-word prefix and inline selected
method slots sized by its normalized declared contract. A null payload
denotes disassociation. Pointer dummies receive its address, so OUT/INOUT
association changes update the caller without exposing a callee-local wrapper.
