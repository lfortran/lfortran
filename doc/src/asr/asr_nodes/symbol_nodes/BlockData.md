# BlockData

A BLOCK DATA program unit: the initial values of COMMON blocks.

## Declaration

### Syntax

```text
BlockData(symbol_table symtab, identifier name,
    identifier? block_data_name, identifier* dependencies,
    location start_name, location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the unit. It owns the variables of its COMMON blocks, the [CommonBlock](CommonBlock.md) view of each block, and everything else its specification part declares. |
| `name` | the key of the unit in the global symbol table. |
| `block_data_name` | the name written in `block data name`, or `nil` for the unnamed BLOCK DATA unit, of which a program has at most one. |
| `dependencies` | the names of the modules the unit uses. |
| `start_name` | the source span of the name in `block data name`. |
| `end_name` | the source span of the name in `end block data name`, or an empty span when the end statement does not repeat it. |

### Return values

None.

## Description

A BLOCK DATA unit has no executable statements and is never called. Its only
effect is the initial values it gives: a DATA statement in it becomes the
initializer (`symbolic_value` and `value`) of its own
[Variable](Variable.md), and each COMMON block it gives values to is a
[CommonBlock](CommonBlock.md) view whose `role` is `Initialized`, also when
every value it gives is zero. A backend defines the storage of each such block
there, with those values; the views of the same block in other scoping units
give no values and only share that storage.

Like [Program](Program.md) and [Module](Module.md), a **BlockData** is owned by
the global symbol table.

## Examples

A BLOCK DATA unit that initializes two blocks, and a program that declares
them:

```{literalinclude} ../../examples/blockdata.asr
:language: clojure
```

## See Also

[CommonBlock](CommonBlock.md), [common_block_role](../enum_nodes/common_block_role.md), [Variable](Variable.md), [Program](Program.md), [Module](Module.md)
