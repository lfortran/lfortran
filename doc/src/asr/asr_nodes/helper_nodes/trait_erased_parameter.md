# trait_erased_parameter

An explicit substitution for a checked generic binder of a provider entry.

## Declaration

### Syntax

```text
trait_erased_parameter = (symbol parameter, symbol? contract,
    trait_erased_operation* operations, ttype? member)
```

`parameter` is the original `Template`'s binder, with its defining scope intact.
For an open binder, `contract` is the canonical runtime contract of its
normalized nominal constraint, `operations` covers each distinct normalized
checked requirement exactly once, and `member` is absent. Equal layouts,
same-spelled traits, and equal constraints on different binder positions do
not identify these records.

For a closed binder of a type-set trait, `member` is exactly one declared member
type, `contract` is absent and there are no operations: the shared instantiator
substitutes the member itself and selects the type set's checked member
witnesses.

See [TraitErasure](../symbol_nodes/TraitErasure.md).
