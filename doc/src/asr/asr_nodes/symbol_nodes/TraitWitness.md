# TraitWitness

Provider-owned runtime evidence for one explicitly selected nominal conformance.

## Declaration

### Syntax

```text
TraitWitness(symbol_table symtab, identifier name, symbol contract,
    symbol implementation, symbol* procedures, identifier* dependencies, abi abi,
    trait_lifecycle lifecycle, symbol* projections)
```

`implementation` identifies the original `TraitImplementation`; its nominal
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
even when additional child methods cannot be dispatched at runtime. The
immutable table appends these
parent-table references after its lifecycle and message entries. A consumer
therefore weakens a view without discovering or reselecting conformance.

The immutable table has strong provider-owned linkage. Imports reference that
table rather than emitting an arbitrary weak alternative. Same-build/target
separate compilation requires the canonical defining scope and symbol name,
not local aliases, layout hashes, symbol-table counters, or ASR addresses.

Verification checks slot signatures and counts, unique provider-owned adapters,
nominal evidence, concrete receiver types, and ordinary argument contracts.
The lifecycle reference must agree with the implementation's nominal type.
Only an adapter named by this witness may use `TraitReceiver` for its receiver.
