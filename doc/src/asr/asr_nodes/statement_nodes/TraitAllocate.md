# TraitAllocate

Initialize previously unallocated scalar trait ownership.

## Declaration

### Syntax

```text
TraitAllocate(expr target, expr? source, symbol? witness, bool copy_value,
    symbol? type_declaration)
```

`target` is a scalar allocatable trait variable. Typed allocation has no
`source`: its explicitly named, visible `type_declaration` must be instantiable
and agree with the selected `witness`. Keeping that reference preserves import
aliases in inspection output.

For SOURCE/MOLD allocation, `source` is an exact nonpolymorphic concrete value
with explicit selected `witness`, or a same-contract borrowed view carrying
its existing witness. `type_declaration` is absent. `copy_value` is true only
for SOURCE; MOLD and typed allocation default-initialize rather than copying
source values.

The operation requires an unallocated target and establishes a fully initialized
three-word owning header with independent payload storage. Failed allocation,
including component allocation, terminates; STAT/ERRMSG are not represented or
silently ignored. No ordinary two-word CLASS wrapper is used as the trait header.
Verification checks owner state category, canonical contract, nominal type and
complete lifecycle evidence before lowering.
