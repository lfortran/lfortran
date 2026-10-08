# trait_projection_slot

An explicit selected-method transfer in a runtime trait projection.

## Declaration

### Syntax

```text
trait_projection_slot = (int source)
```

At index `i` of `TraitProject.slots`, `source` is the nonnegative slot index in
the source view that supplies target slot `i`. Verification checks the index,
all original nominal origins, and the ordinary callable signature after proving
the target contract from the source's declared ancestry.

LLVM copies that method address into the target descriptor's own inline slot.
It does not infer a binding or construct a provider-side table for the target.
