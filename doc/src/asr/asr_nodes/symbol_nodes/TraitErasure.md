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

See [traits](../../traits.md), [TraitWitness](TraitWitness.md), and
[trait_erased_parameter](../helper_nodes/trait_erased_parameter.md).
