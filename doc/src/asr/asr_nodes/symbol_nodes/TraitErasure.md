# TraitErasure

One reusable erased entry derived from a definition-time-checked generic body.

## Declaration

### Syntax

```text
TraitErasure(symbol_table symtab, identifier name, symbol generic,
    symbol procedure, trait_erased_parameter* parameters)
```

`generic` references the existing `Template`, not an AST body or a new generic
engine. `procedure` is an ordinary function in the owned provider scope.
`parameters` records each original scoped binder, its exact nominal runtime
contract, and complete operation substitutions.

The shared template instantiator builds this signature and body using explicit
borrowed `TraitObjectType` substitutions. It removes declaration-only binder
aliases, not actual dummies. Operation wrappers dynamically invoke the supplied
argument view's canonical slots. All signatures and forwarded dependencies are
bound before bodies are copied. The checked source remains available for normal
static specialization.

Verification checks scope, original binder identities, exact nominal domains,
entry signature, operation coverage, and wrappers that forward their own
arguments in order. Module externalization marks the owned functions as external,
so clients reference the compiled entry rather than rebuilding it.

The provider descriptor and each generic argument descriptor are independent.
This metadata introduces no concrete type layout, ownership, finalization or
runtime specialization cache. The initial subset is readonly scalar generic
arguments and scalar integer results. Static-only backends may ignore the
owned runtime routines; executable runtime use requires LLVM support.

A generic whose binders are all closed type-set traits instead has one member
entry per member tuple of its contract's family. Each parameter then records
its exact `member` type and no contract or operations, and `procedure` is the
same checked body instantiated by the same engine at that tuple, with the type
set's checked member witnesses. Member entries are concrete: their arrays,
locals and `T` results are ordinary intrinsic storage. A provider module owns
at most one entry per generic and tuple, registered as that scope's
specialization before any body is copied, so self-recursion reuses it. The
verifier checks the declared membership, the exact member signature and
uniqueness; a witness slot of that member requires the provider's entry.
Clients only select a member slot; they never create or instantiate entries.

See [traits](../../traits.md), [TraitWitness](TraitWitness.md), and
[trait_erased_parameter](../helper_nodes/trait_erased_parameter.md).
