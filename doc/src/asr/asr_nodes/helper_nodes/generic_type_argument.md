# generic_type_argument

The concrete type argument of one parameter of a generic derived type's
instance.

## Declaration

### Syntax

```text
generic_type_argument = (ttype type, symbol? type_declaration, symbol* evidence)
```

### Arguments

| Argument | Description |
| --- | --- |
| `type` | The concrete scalar type that replaces the parameter: an intrinsic type, or the nonpolymorphic `StructType` of a derived type, which may itself be an instance. |
| `type_declaration` | The derived type's `Struct`, visible from the instance's scope; present if and only if `type` is a `StructType`. |
| `evidence` | The `TraitImplementation` of the argument to each universal-trait constraint of the parameter, in constraint order, visible from the instance's scope. |

### Return values

None.

## Description

The evidence is selected where the instance is created, among the
conformances visible there, and is part of the instance's identity: equal
types with agreeing conformances denote one instance. A conformance of another
module is referenced through an import. An argument that is an instance its
module or program has not completed yet records its evidence when that
instance is completed.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[struct_generic](struct_generic.md),
[TraitImplementation](../symbol_nodes/TraitImplementation.md)
