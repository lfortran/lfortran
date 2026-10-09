# type_set_requirement

A serializable, all-member certificate for an intrinsic numeric operation.

## Declaration

### Syntax

```text
type_set_requirement = (symbol procedure, type_set_operation operation,
    type_set_witness* witnesses)
```

`procedure` is a bodyless interface `Function` with `is_restriction = true`
owned by the constraint's template. Its scalar signature uses the constraint's
type parameter or a common concrete type. It has no implicit receiver.
`operation` describes the intrinsic operation, not a generated procedure name.

The witness family contains exactly one member of the trait's finite type set
per entry. Verification checks substituted argument and result types and the
recorded operation in each concrete witness body. Missing, repeated, outside,
wrong-kind, or wrong-operation witnesses are invalid ASR.

Generic expressions use ordinary `FunctionCall` nodes targeting this
restriction. The shared template instantiator binds it to the selected concrete
witness before copying the generic body.

See [TraitConstraint](../symbol_nodes/TraitConstraint.md),
[type_set_operation](type_set_operation.md), and [type_set_witness](type_set_witness.md).
