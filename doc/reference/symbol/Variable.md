<!-- This is an automatically generated file. Do not edit it manually. -->

# Variable

Variable, a **symbol** node.

## Declaration

### Syntax

Variable(symbol_table parent_symtab, identifier name, identifier* dependencies, intent intent, expr? symbolic_value, expr? value, storage_type storage, ttype type, symbol? type_declaration, abi abi, access access, presence presence, bool value_attr, bool target_attr, bool contiguous_attr, string? bindc_name, bool is_volatile, bool is_protected)

### Arguments
Input arguments are `parent_symtab` of type symbol_table, `name` of type identifier, `dependencies` of type identifier*, `intent` of type intent, `symbolic_value` of type expr?, `value` of type expr?, `storage` of type storage_type, `type` of type ttype, `type_declaration` of type symbol?, `abi` of type abi, `access` of type access, `presence` of type presence, `value_attr` of type bool, `target_attr` of type bool, `contiguous_attr` of type bool, `bindc_name` of type string?, `is_volatile` of type bool, `is_protected` of type bool.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Variable::m_parent_symtab cannot be nullptr
* Variable not found in parent_symtab symbol table
* Variable's parent symbol table does not point to it
* Variable's parent-symbolTable and actuall parent symbolTable don't match (Maybe inserted from another symbolTable)
* Variable::m_parent_symtab must be present in the ASR ()
* Initialisation ofmust reduce to a compile time constant.
* Initialisation ofmust reduce to a compile time constant.
* Variabledoesn't depend onbut is found in its dependency list.
* Variabledepends onbut isn't found in its dependency list.