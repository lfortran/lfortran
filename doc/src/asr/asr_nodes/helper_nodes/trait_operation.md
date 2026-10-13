# trait_operation

An initializer or operator requirement of a universal trait.

## Declaration

### Syntax

```text
trait_operation = (symbol member, type_set_operation operation)
```

`member` is a nongeneric interface `Function` directly declared in the
trait's symbol table, with no receiver, whose signature names the
implementing type `itself`. `operation` is `TypeSetConversion` for an
initializer, which returns `itself`, or `TypeSetBinary` or
`TypeSetComparison` for an operator requirement of two operands, at least
one of type `itself`. A member has at most one operation.

A binder's `TraitConstraint` requirement for the member is the member's
signature with the binder for `itself` and still no receiver. A
`TraitImplementation` binds it with `is_nopass` to a procedure of the
implementing type whose signature matches with `itself` replaced by that
type, or, for an intrinsic type, to a procedure of the conformance's unit
that applies the intrinsic conversion or operation. Generic bodies call the
requirement with ordinary `FunctionCall` nodes, which specialization binds to
those procedures.

See [Trait](../symbol_nodes/Trait.md) and
[type_set_operation](type_set_operation.md).
