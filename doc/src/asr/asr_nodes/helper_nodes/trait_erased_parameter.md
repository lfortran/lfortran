# trait_erased_parameter

An explicit borrowed-view substitution for a checked generic binder.

## Declaration

### Syntax

```text
trait_erased_parameter = (symbol parameter, symbol contract,
    trait_erased_operation* operations)
```

`parameter` is the original `Template`'s binder, with its defining scope intact.
`contract` is the canonical runtime contract of its normalized nominal
constraint. `operations` covers each distinct normalized checked requirement
exactly once. Equal layouts, same-spelled traits, and equal constraints on
different binder positions do not identify these records.

See [TraitErasure](../symbol_nodes/TraitErasure.md).
