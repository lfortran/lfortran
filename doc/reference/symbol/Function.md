# Function

## ASR

<!-- BEGIN AUTO: asr -->
```
Function(symbol_table symtab, identifier name, ttype function_signature, identifier* dependencies, expr* args, stmt* body, expr? return_var, access access, bool deterministic, bool side_effect_free, string? module_file, location start_name, location end_name)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `Function::m_symtab` cannot be `nullptr`
* The `Function::m_symtab`->parent is not the right parent
* The `X::m_symtab::asr_owner` must point to X
* `Function::m_symtab`->counter must be unique
* The asr_owner invariant failed
* Function name is required
* Type signature is required for `[...]`
* Dependency [...] is inside symbol table [...]
* Function [...] doesn't depend on [...] but is found in its dependency list.
* Function [...] depends on [...] but isn't found in its dependency list.
* Number of argument types in FunctionType must be exactly same as number of arguments in the function
<!-- END AUTO: verify -->
