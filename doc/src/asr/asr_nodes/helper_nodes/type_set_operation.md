# type_set_operation

Non-executable metadata identifying a finite type set's required operation.

## Declaration

### Syntax

```text
type_set_operation
    = TypeSetBinary(binop op)
    | TypeSetComparison(cmpop op)
    | TypeSetConversion()
```

`TypeSetBinary` currently covers scalar same-binder `+`, `-`, `*`, and `/`.
`TypeSetComparison` records the ordinary comparison operator; its common
result is logical. In particular, complex members cannot prove ordering.

`TypeSetConversion` converts its scalar source to the enclosing constraint's
member type. Concrete witnesses use `int`, `real`, or `cmplx` with the target's
kind, subject to the ordinary intrinsic factory's applicability checks.

The descriptor is not an executable expression. Existing restriction
`FunctionCall` nodes represent generic operations, and ordinary typed calls,
operators, intrinsics, and casts represent concrete ones.

See [type_set_requirement](type_set_requirement.md).
