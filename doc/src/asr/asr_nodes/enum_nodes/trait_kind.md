# trait_kind

## Declaration

### Syntax

```text
trait_kind = UniversalTrait | IntrinsicTypeSet
```

`UniversalTrait` describes nominal receiver-independent method requirements.
It has no finite member types.

`IntrinsicTypeSet` is a finite, nonempty set of concrete scalar numeric
categories and kinds. Membership provides conformance without user
implementation records. It is constraint-only; runtime objects and concrete
union variables are not supported.

See [Trait](../symbol_nodes/Trait.md).
