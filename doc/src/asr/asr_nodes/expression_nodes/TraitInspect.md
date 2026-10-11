# TraitInspect

Expose a scalar runtime trait's concrete identity and payload as an ordinary
unlimited-polymorphic inspection view.

## Declaration

### Syntax

```text
TraitInspect(expr view, symbol type_declaration, ttype type)
```

`view` is an explicitly borrowed `TraitObjectType`, not a pointer or allocation
slot. `type_declaration` names an in-scope unlimited-polymorphic declaration and
`type` is its scalar, nonowning `StructType`. This expression has no constant
value. It cannot claim any particular concrete type.

## Semantics

The result contains the original concrete vptr and payload address. It copies
no payload, changes no conformance, and owns no value. In SELECT TYPE lowering,
an identity `TraitProject` first captures the source's complete selected
descriptor. `TraitBorrow` checks pointer association or allocation state before
that capture. Reassociation of the original source does not retarget inspection.

Existing `SelectType` guards inspect the ordinary concrete nominal metadata.
Their explicit `ClassToStruct` and `ClassToClass` casts bind nonowning
`Association` variables. TYPE IS is exact identity; CLASS IS follows real
concrete ancestry, ordered from most specific to least specific.

The verifier checks the borrowed source and ordinary result declaration,
guarded narrowing, and associate-name attributes. Binary, module and ASR text
formats preserve this node; Fortran inspection prints `trait_inspect(...)`.
Executable backends other than LLVM retain the runtime-trait capability
diagnostic. LLVM lowers the two fields explicitly rather than bitcasting the
expanded trait descriptor.

## See Also

[Experimental traits](../../traits.md),
[storage_type](../enum_nodes/storage_type.md), [Cast](Cast.md).
