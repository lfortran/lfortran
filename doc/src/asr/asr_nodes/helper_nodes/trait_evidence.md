# trait_evidence

The conformance that satisfies one nominal constraint of a generic requirement
for a concrete type argument.

## Declaration

### Syntax

```text
trait_evidence = (symbol constraint, symbol implementation)
```

### Arguments

| Argument | Description |
| --- | --- |
| `constraint` | A nominal `TraitConstraint` of the generic requirement, on one of its type parameters. |
| `implementation` | The `TraitImplementation` of that parameter's concrete type argument to the constraint's trait, selected where the generic body made the call; an import in the specialization when it belongs to another module. |

### Return values

None.

## Description

A specialization of a generic requirement whose type arguments are concrete has
one entry for every nominal constraint of the generic requirement; a
specialization with forwarded type arguments has none and takes its caller's
conformances. The implementation must bind every method of the constraint. The
same type arguments with different evidence are distinct specializations.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[trait_requirement](trait_requirement.md),
[TraitConstraint](../symbol_nodes/TraitConstraint.md)
