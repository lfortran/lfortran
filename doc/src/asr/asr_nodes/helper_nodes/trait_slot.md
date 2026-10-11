# trait_slot

One callable slot and all of its nominal message obligations.

## Declaration

### Syntax

```text
trait_slot = (symbol* origins, symbol procedure, ttype* type_arguments)
```

`origins` lists original trait members in canonical hierarchy order. Compatible
same-name signatures share one slot but do not lose their independent
conformance obligations. A shared ancestor in a diamond appears only once.
`procedure` is a contract-owned interface with the view prepended to the
receiver-independent message signature.
Generic origins are `Template` symbols with scoped binder identity. When every
binder is open (constrained by one nominal trait), the single slot procedure is
an explicitly erased ordinary interface, not a finite family of specializations.
Its generic scalar arguments are independent borrowed views of their nominal
constraints, and `type_arguments` is empty.

When every binder is closed (constrained by exactly one type-set trait), the
callable instead has one slot per member tuple: binders in `Template` argument
order, members in the type-set trait's declared order. All slots of this
family share the same `origins`; `type_arguments` is the tuple, and `procedure`
is the message signature instantiated at exactly that tuple. The members come
only from the type-set trait declaration, never from visible conformances or
client types. Other slots have empty `type_arguments`. Projections map slots by
origin coverage and equal `type_arguments`.

See [TraitRuntimeContract](../symbol_nodes/TraitRuntimeContract.md).
