# type_set_witness

One concrete member's proof of an intrinsic capability.

## Declaration

### Syntax

```text
type_set_witness = (ttype member_type, symbol procedure)
```

`member_type` is the exact concrete scalar intrinsic category and kind, and
for character its constant length.
`procedure` is an ordinary implementation `Function` owned by the template.
Its concrete arguments and result match the restriction after substituting
that member. Its body assigns exactly the recorded operation to its result;
an arbitrary function with the same signature is not a valid witness.

Conversion witnesses use the actual intrinsic conversion's result kind.
Equivalent identity and canonical cast forms are also verifiable, but logical
sources and incorrect cast kinds are not numeric-conversion proofs, and only a
logical source proves a conversion to a logical member.

These functions are definition metadata inside an uninstantiated template.
Only the selected member's ordinary function is copied into executable scopes;
no backend chooses membership or repairs types.

See [type_set_requirement](type_set_requirement.md).
