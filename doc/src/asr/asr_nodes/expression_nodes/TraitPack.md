# TraitPack

Create a call-duration borrowed view from exact concrete scalar storage.

## Declaration

### Syntax

```text
TraitPack(expr payload, symbol witness, ttype type)
```

`payload` is an existing nonpolymorphic derived-type designator. Its nominal
declaration must match the selected visible witness, not merely its structural
layout. `type` is that witness's `TraitObjectType`.

AST-to-ASR selects conformance under the normal explicit implementation-module
visibility policy and diagnoses conflicting witnesses. LLVM fills a stack
header without allocating, copying, or finalizing the payload. Forwarding an
existing view of the same contract uses `Var`, not another `TraitPack`.
Anonymous construction retains the selected evidence for every original
nominal requirement. Known-parent/subset weakening uses `TraitProject`, not a
new pack. Unknown polymorphic actuals remain unsupported.
