# symbol

The symbols of ASR.

## Declaration

### Syntax

```text
symbol
    = Program(symbol_table symtab, identifier name, identifier* dependencies, stmt* body, location start_name, location end_name)
    | Module(symbol_table symtab, identifier name, identifier? parent_module, identifier* dependencies, bool loaded_from_mod, bool intrinsic, bool has_submodules, location start_name, location end_name)
    | Function(symbol_table symtab, identifier name, ttype function_signature, identifier* dependencies, expr* args, stmt* body, expr? return_var, access access, bool deterministic, bool side_effect_free, string? module_file, location start_name, location end_name)
    | GenericProcedure(symbol_table parent_symtab, identifier name, symbol* procs, access access)
    | CustomOperator(symbol_table parent_symtab, identifier name, symbol* procs, access access)
    | ExternalSymbol(symbol_table parent_symtab, identifier name, symbol external, identifier module_name, identifier* scope_names, identifier original_name, access access)
    | Struct(symbol_table symtab, identifier name, ttype struct_signature, identifier* dependencies, identifier* members, identifier* member_functions, abi abi, access access, bool is_packed, bool is_abstract, bool is_sequence, call_arg* initializers, expr? alignment, symbol? parent, identifier* kind_params)
    | Enum(symbol_table symtab, identifier name, identifier* dependencies, identifier* members, abi abi, access access, enumtype enum_value_type, ttype type, symbol? parent)
    | Union(symbol_table symtab, identifier name, identifier* dependencies, identifier* members, abi abi, access access, call_arg* initializers, symbol? parent)
    | Variable(symbol_table parent_symtab, identifier name, identifier* dependencies, intent intent, expr? symbolic_value, expr? value, storage_type storage, ttype type, symbol? type_declaration, abi abi, access access, presence presence, bool value_attr, bool target_attr, bool contiguous_attr, string? bindc_name, bool is_volatile, bool is_protected, pass_attr pass_attr, identifier? self_argument, codimension* codims)
    | StructMethodDeclaration(symbol_table parent_symtab, identifier name, identifier? self_argument, identifier proc_name, symbol proc, abi abi, bool is_deferred, bool is_nopass)
    | AssociateBlock(symbol_table symtab, identifier name, stmt* body)
    | Block(symbol_table symtab, identifier name, stmt* body)
    | Requirement(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)
    | Template(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)
    | Namelist(symbol_table parent_symtab, identifier group_name, symbol* var_list)
```

### Arguments

None.

### Return values

None.

## Description

A symbol is an entry of a symbol table. Every symbol carries its `name`, so
that a pointer to one is enough to report what it is, and either a `symtab` it
owns or the `parent_symtab` it is stored in; only one of the two is present,
because the parent is reachable from a symbol table it owns.

The symbols divide into:

- program units: [Program](Program.md), [Module](Module.md) and
  [Function](Function.md);
- type definitions: [Struct](Struct.md), [Enum](Enum.md) and [Union](Union.md),
  with [StructMethodDeclaration](StructMethodDeclaration.md) for a type-bound
  procedure;
- data: [Variable](Variable.md) and [Namelist](Namelist.md);
- names for other symbols:
  [GenericProcedure](GenericProcedure.md), [CustomOperator](CustomOperator.md)
  and [ExternalSymbol](ExternalSymbol.md);
- scopes that are not program units: [Block](Block.md) and
  [AssociateBlock](AssociateBlock.md);
- generics: [Requirement](Requirement.md) and [Template](Template.md).

A symbol declared in one module and used in another is reached through an
[ExternalSymbol](ExternalSymbol.md) in the using scope, so every symbol an
expression names is reachable from the symbol table of the program unit it
appears in.

## See Also

[ttype](../type_nodes/ttype.md), [TranslationUnit](../unit_nodes/TranslationUnit.md),
[ASR overview](../../asr.md)

## Symbol Nodes

