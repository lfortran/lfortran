# trait_kind

## Declaration

### Syntax

```text
trait_kind = UniversalTrait | IntrinsicTypeSet
```

`UniversalTrait` describes nominal method requirements, independent of any
implementing type.
It has no finite member types.

`IntrinsicTypeSet` is a finite, nonempty set of concrete scalar intrinsic
types: integer, real, complex and logical types of supported kinds and
character types of a supported kind and a constant length. Membership provides
conformance without user implementation records. It is constraint-only; runtime objects and concrete
union variables are not supported.

See [Trait](../symbol_nodes/Trait.md).
