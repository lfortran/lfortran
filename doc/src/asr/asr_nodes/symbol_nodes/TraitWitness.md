# TraitWitness

Selected nominal evidence and typed runtime adapters for concrete construction.

## Declaration

### Syntax

```text
TraitWitness(symbol_table symtab, identifier name, symbol contract,
    symbol? implementation, symbol* procedures, identifier* dependencies, abi abi,
    trait_lifecycle lifecycle, symbol* projections, symbol* components)
```

For a named contract, `implementation` identifies the original `TraitImplementation`; its nominal
concrete type is independent of the contract's interface hierarchy.
`procedures` has one ordinary typed adapter for every canonical contract slot.
Those adapters belong to the witness's own symbol table, rather than appearing
as unrelated module procedures. Their ownership authorizes receiver recovery
and lets static-only backends leave runtime evidence unmaterialized.
`dependencies` retains those adapters even when the provider never constructs a
view. Each adapter reuses normal call construction and explicit PASS/NOPASS
receiver normalization.
`abi` records definition availability: `Source` emits the provider definition,
and `ExternalUndefined` references separately compiled evidence. Ordinary module
import externalization updates this field and the owned implementation functions.
`lifecycle` preserves the implementation's canonical nominal concrete type.
Its descriptor belongs to that concrete type, independently of this selected
conformance; multiple witnesses do not introduce multiple payload finalizers.
See [trait_lifecycle](../helper_nodes/trait_lifecycle.md).

`projections` retains one provider-owned witness per direct parent of the
declared contract, in parent declaration order. A projected witness uses the
same original implementation and concrete lifecycle, but its own correctly
typed adapters for that parent's messages. Eligible parent witnesses may exist
even when additional child methods cannot be dispatched at runtime. These are
compile-time selected-evidence references; physical tables no longer need
parent-table links.

For direct concrete construction of an anonymous conjunction, `implementation`
is absent and `components` retains one original named witness for each normalized
nominal requirement. All components must have the same concrete lifecycle.
Overlapping and independently coalesced bindings must agree on both procedure
and receiver. The existing adapter builder creates correctly typed adapters
from these selected bindings; no new generic specialization mechanism is used.
This construction-time evidence is never rediscovered when projecting an
already erased view.

The immutable table has strong provider-owned linkage. Imports reference that
table rather than emitting an arbitrary weak alternative. Same-build/target
separate compilation requires the canonical defining scope and symbol name,
not local aliases, layout hashes, symbol-table counters, or ASR addresses.

Verification checks slot signatures and counts, unique provider-owned adapters,
nominal evidence, concrete receiver types, and ordinary argument contracts.
The lifecycle reference must agree with the implementation's nominal type.
Only an adapter named by this witness may use `TraitReceiver` for its receiver.
The adapter may access that erased argument only through `TraitReceiver`, not
redispatch through or save its original view layout. This verified common-prefix
protocol makes its method pointer transferable to a smaller projected descriptor.

The immutable table contains lifecycle and method addresses used at concrete
construction. Every runtime view copies its selected method addresses into its
own inline slots, so even late-client conjunctions need no additional provider
table or escaping stack table.
