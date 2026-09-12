# ASR Symbol Nodes

A symbol is something a symbol table maps a name to: a program unit, a
procedure, a type definition, a variable. Every symbol either owns a symbol
table of its own (`symtab`) or records the one it is stored in
(`parent_symtab`), and carries its `name` so that a pointer to it is enough to
report what it is.

```{toctree}
---
maxdepth: 1
---
AssociateBlock
Block
CustomOperator
Enum
ExternalSymbol
Function
GenericProcedure
Module
Namelist
Program
Requirement
Struct
StructMethodDeclaration
symbol
Template
Union
Variable
```
