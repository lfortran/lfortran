# Namelist

A namelist group.

## Declaration

### Syntax

```text
Namelist(symbol_table parent_symtab, identifier group_name,
    symbol* var_list)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this group is stored in. |
| `group_name` | the name of the group, the name written in `nml=name`. |
| `var_list` | the variables of the group, in declaration order. |

### Return values

None.

## Description

A namelist group names a list of variables so that
[FileRead](../statement_nodes/FileRead.md) and
[FileWrite](../statement_nodes/FileWrite.md) can transfer all of them at once,
matching each item in the file by name. The `nml` member of those statements
points at this symbol.

The group is a symbol because it is looked up by name and because its members
are the variable symbols themselves: namelist input assigns to them by name at
run time.

## Examples

```clojure
(Namelist
  :parent_symtab 1
  :group_name "nml"
  :var_list [
    (SymbolRef 1 "x")
    (SymbolRef 1 "y")
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/namelist.asr
:language: clojure
```

## See Also

[FileRead](../statement_nodes/FileRead.md), [FileWrite](../statement_nodes/FileWrite.md), [Variable](Variable.md)
