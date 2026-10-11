# CommonBlock

The view of one COMMON block that one scoping unit declares.

## Declaration

### Syntax

```text
CommonBlock(symbol_table parent_symtab, identifier name,
    identifier? block_name, identifier link_name, symbol* members,
    common_association* associations, abi abi, storage_type storage,
    common_block_role role)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table of the scoping unit that declares the block, or the global symbol table for a block declared interactively at the top level. It also owns every variable the view places. |
| `name` | the key of the view in `parent_symtab`. It is distinct from any Fortran name, because a block may have the name of a local entity, such as one of its own variables. |
| `block_name` | the name of the block as written between the slashes, or `nil` for blank COMMON. |
| `link_name` | the global symbol of the storage of the block. Every view of the same block, in any scoping unit of any translation unit, has the same `link_name`. |
| `members` | the variables that the COMMON statements of the unit give the block, in order. They are laid out one after another from the start of the block. |
| `associations` | the other variables of the unit that EQUIVALENCE places in the block; see [common_association](../helper_nodes/common_association.md). |
| `abi` | `Source`, or `BindC` for a block with the `bind(c)` attribute. |
| `storage` | `Save` when the unit gives the block the save attribute, `Default` otherwise. |
| `role` | how the view provides the storage of the block; see [common_block_role](../enum_nodes/common_block_role.md). |

### Return values

None.

## Description

Fortran associates the COMMON blocks of a whole program by their names, but
every scoping unit that declares a block gives it a layout of its own: its own
variables, in its own order, of its own types. A **CommonBlock** is that layout
for one scoping unit. Each variable it places points back to it through
`common_block` (see [Variable](Variable.md)), so everything a backend needs to
address the variable is in the variable's own symbol table: its position comes
from `members` and `associations`, and the storage from `link_name`.

Views are never matched with each other by the names of their variables. Two
units may give one name to variables at different positions of a block, or
different names to the variable at one position; the storage is shared through
`link_name` alone, and a variable is associated with whatever the other views
place at the same position. A backend combines all views of one `link_name`
that a translation unit contains into one global, as large as the largest of
them: a named block has the same size in every scoping unit, but blank COMMON
may differ.

A module variable in a COMMON block is used from other scoping units like any
other module variable, through an [ExternalSymbol](ExternalSymbol.md) to the
[Variable](Variable.md), whose `common_block` is the view of the module. A unit
that uses the module and also declares the block has a view of its own. A
module file stores the views of the module like any other symbol, so a unit
compiled later sees the layout the module was compiled with, whatever other
units have declared since.

In interactive use, the first cell that declares a block defines its storage.
A later cell refers to that storage through an `External` view, which cannot
make the block larger or give it initial values.

The views in the ASR are the only persistent record of a block: the frontend
makes up no module, derived type or module file to hold it, and keeps no side
table of layouts across scoping units. A backend derives the layout of the
storage of each `link_name` locally, from the views it emits.

## Examples

The view of `common /blk/ n, a` in a program that also has
`equivalence (e(1), n)`:

```clojure
(CommonBlock
  :parent_symtab 1
  :name "common~blk"
  :block_name "blk"
  :link_name "__module_file_common_block_blk_struct_instance_blk"
  :members [
    (SymbolRef 1 "n")
    (SymbolRef 1 "a")
  ]
```

Each variable it places points back to it:

```clojure
:common_block (SymbolRef 1 "common~blk")
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/commonblock.asr
:language: clojure
```

## See Also

[common_association](../helper_nodes/common_association.md), [common_block_role](../enum_nodes/common_block_role.md), [BlockData](BlockData.md), [Variable](Variable.md), [ExternalSymbol](ExternalSymbol.md)
