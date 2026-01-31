# Function

Function, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Function(symbol_table symtab, identifier name, ttype function_signature, identifier* dependencies, expr* args, stmt* body, expr? return_var, access access, bool deterministic, bool side_effect_free, string? module_file, location start_name, location end_name)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `symtab` of type `symbol_table`, `name` of type `identifier`, `function_signature` of type `ttype`, `dependencies` of type `identifier*`, `args` of type `expr*`, `body` of type `stmt*`, `return_var` of type `expr?`, `access` of type `access`, `deterministic` of type `bool`, `side_effect_free` of type `bool`, `module_file` of type `string?`, `start_name` of type `location`, `end_name` of type `location`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* The Function::m_symtab cannot be nullptr
* The Function::m_symtab->parent is not the right parent
* The X::m_symtab::asr_owner must point to X
* Function::m_symtab->counter must be unique
* The asr_owner invariant failed
* Function name is required
* Type signature is required for `[...]`
* Dependency [...] is inside symbol table [...]
* Function [...] doesn't depend on [...] but is found in its dependency list.
* Function [...] depends on [...] but isn't found in its dependency list.
* Number of argument types in FunctionType must be exactly same as number of arguments in the function
<!-- END AUTO: restrictions -->
