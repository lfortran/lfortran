# common_block_role

How a [CommonBlock](../symbol_nodes/CommonBlock.md) view provides the storage of
its block.

## Declaration

### Syntax

```text
common_block_role = Tentative | Initialized | External
```

### Values

| Value | Meaning |
|----------|-------------|
| `Tentative` | the view gives no initial value: none of its variables has an initializer. Its storage is a tentative definition, which a linker for a target with common symbols merges with every other definition of the same `link_name`. |
| `Initialized` | the view gives initial values through the initializers of its variables, even if they are all zero, as the views of a [BlockData](../symbol_nodes/BlockData.md) unit do. It defines the storage with those values. |
| `External` | the view only refers to storage that another object file defines. A module read from a module file has such views when the module has object code of its own, as with separate compilation: the object file of the module defines the storage. It is also the view of a block whose storage an earlier interactive cell defined. |

### Return values

None. An enumeration value is not evaluated.

## Description

The role decides what a backend emits for a view, independently of the values
it happens to give: an `Initialized` view whose values are all zero still
defines the storage, so that it is not mistaken for a `Tentative` one.

## Examples

`data z / 0 /` in a BLOCK DATA unit makes its view of `/zero/` `Initialized`,
although the only value it gives is zero:

```clojure
"common~zero" (CommonBlock
  :parent_symtab 2
  :name "common~zero"
  :block_name "zero"
  :link_name "__module_file_common_block_zero_struct_instance_zero"
  :members [
    (SymbolRef 2 "z")
  ]
  :associations []
  :abi :Source
  :storage :Default
  :role :Initialized
)
```

The program's view of the same block gives no value, so it is `Tentative`.
Both come from this complete ASR text document:

```{literalinclude} ../../examples/blockdata.asr
:language: clojure
```

## See Also

[CommonBlock](../symbol_nodes/CommonBlock.md), [BlockData](../symbol_nodes/BlockData.md), [storage_type](storage_type.md)
