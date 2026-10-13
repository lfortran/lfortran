# type_set_operation

Non-executable metadata identifying a finite type set's required operation.

## Declaration

### Syntax

```text
type_set_operation
    = TypeSetBinary(binop op)
    | TypeSetComparison(cmpop op)
    | TypeSetConversion()
    | TypeSetLogical(logicalbinop op)
    | TypeSetConcat()
```

`TypeSetBinary` currently covers scalar same-binder `+`, `-`, `*`, and `/`.
`TypeSetComparison` records the ordinary comparison operator; its common
result is logical. In particular, complex members cannot prove ordering, and
logical members prove no comparison, since standard Fortran compares logical
values with `.eqv.` and `.neqv.`. `TypeSetLogical` covers scalar same-binder
`.and.`, `.or.`, `.eqv.` and `.neqv.` of logical members, and `TypeSetConcat`
the concatenation `//` of character members, whose result is the character
type of twice the member's length.

`TypeSetConversion` converts its scalar source to the enclosing constraint's
member type. Concrete witnesses use `int`, `real`, `cmplx` or `logical` with
the target's kind, subject to the ordinary intrinsic factory's applicability
checks; character members have no conversion.

The descriptor is not an executable expression. Existing restriction
`FunctionCall` nodes represent generic operations, and ordinary typed calls,
operators, intrinsics, and casts represent concrete ones.

A universal trait's [trait_operation](trait_operation.md) uses the same
descriptor for its initializer and operator requirements.

See [type_set_requirement](type_set_requirement.md).
