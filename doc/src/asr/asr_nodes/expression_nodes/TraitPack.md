# TraitPack

Create a call-duration borrowed view from exact concrete scalar storage.

## Declaration

### Syntax

```text
TraitPack(expr payload, symbol witness, ttype type)
```

`payload` is an existing nonpolymorphic derived-type designator, or, for a
procedure's view actual, a value of its statement: a structure constructor or
a nonallocatable, nonpointer function result, which the passes materialize for
the duration of the call before code generation. A named constant, or a
component or element that carries a value folded from one, is not storage, and
the verifier rejects such a designator, like a structure constant, as a
payload. AST-to-ASR passes a named constant, or a component of one, as the
structure constructor of its value, derived from the named constant itself;
such an element of a named constant array, or a component of one, is
diagnosed as not implemented yet. The payload's nominal declaration must match
the selected visible witness, not merely its structural layout. `type` is that
witness's `TraitObjectType`.

AST-to-ASR selects conformance under the normal explicit implementation-module
visibility policy and diagnoses conflicting witnesses. LLVM fills a stack
header without allocating, copying, or finalizing the payload. Forwarding an
existing view of the same contract uses `Var`, not another `TraitPack`.
Anonymous construction retains the selected evidence for every original
nominal requirement. Known-parent/subset weakening uses `TraitProject`, not a
new pack. Unknown polymorphic actuals remain unsupported.
