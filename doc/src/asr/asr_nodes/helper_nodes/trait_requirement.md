# trait_requirement

A trait message and its normalized generic procedure.

## Declaration

### Syntax

```text
trait_requirement = (symbol member, symbol procedure)
```

### Arguments

| Argument | Description |
| --- | --- |
| `member` | Canonical reference to a receiver-independent trait signature. |
| `procedure` | Abstract procedure in the generic scope, with an explicit receiver prepended. |

### Return values

None.

## Description

The mapping retains the source contract while allowing existing procedure
substitution machinery to specialize the checked generic body.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](../symbol_nodes/TraitConstraint.md)
