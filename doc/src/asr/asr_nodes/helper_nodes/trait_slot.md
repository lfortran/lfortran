# trait_slot

One callable slot and all of its nominal message obligations.

## Declaration

### Syntax

```text
trait_slot = (symbol* origins, symbol procedure)
```

`origins` lists original trait members in canonical hierarchy order. Compatible
same-name signatures share one slot but do not lose their independent
conformance obligations. A shared ancestor in a diamond appears only once.
`procedure` is a contract-owned interface with the view prepended to the
receiver-independent message signature.
Generic origins are `Template` symbols with scoped binder identity. Their slot
procedure is an explicitly erased ordinary interface, not a finite family of
specializations. Its generic scalar arguments are independent borrowed views
of their nominal constraints.

See [TraitRuntimeContract](../symbol_nodes/TraitRuntimeContract.md).
