# trait_requirement

A trait message and its normalized generic procedure.

## Declaration

### Syntax

```text
trait_requirement = (symbol member, symbol procedure, trait_evidence* evidence)
```

### Arguments

| Argument | Description |
| --- | --- |
| `member` | Canonical reference to a receiver-independent trait signature. |
| `procedure` | Abstract procedure in the generic scope, with an explicit receiver prepended. |
| `evidence` | For a specialization of a generic requirement with concrete type arguments, the conformance selected for each nominal constraint of the generic requirement; empty otherwise. |

### Return values

None.

## Description

The mapping retains the source contract while allowing existing procedure
substitution machinery to specialize the checked generic body. A published
specialization of a generic requirement keeps, in `evidence`, the conformances
visible where the generic body made the call, so that every instantiation uses
them instead of the conformances visible to its client.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](../symbol_nodes/TraitConstraint.md),
[trait_evidence](trait_evidence.md)
