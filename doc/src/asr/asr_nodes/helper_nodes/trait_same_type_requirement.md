# trait_same_type_requirement

An explicit associated-type equality of a generic trait constraint.

## Declaration

### Syntax

```text
trait_same_type_requirement = (symbol associated_type, ttype type,
    symbol? type_declaration)
```

### Arguments

| Argument | Description |
| --- | --- |
| `associated_type` | Associated declaration in the constraint's required trait hierarchy. |
| `type` | Scalar, nonpolymorphic RHS type of the explicit equality. |
| `type_declaration` | Visible nominal derived type or deferred type declaration for the RHS; absent for intrinsic types. |

### Return values

None.

## Description

The owning `TraitConstraint` records at most one equality per associated
declaration. The RHS has no array, pointer or allocatable wrappers. Intrinsic
RHS types have concrete kinds (and constant length for character); nominal
and deferred RHS types carry matching declaration metadata visible in the
generic scope.

Equalities of all constraints on one binder must agree. They supply the
effective types used to check normalized generic requirements and are checked
against the associated types of the selected conformance at specialization.
They do not infer further constraints or remove the binder's private
`TraitAssociatedType` mappings.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitConstraint](../symbol_nodes/TraitConstraint.md),
[TraitAssociatedType](../symbol_nodes/TraitAssociatedType.md)
