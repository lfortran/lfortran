# Variable

## ASR

<!-- BEGIN AUTO: asr -->
```
Variable(symbol_table parent_symtab, identifier name, identifier* dependencies, intent intent, expr? symbolic_value, expr? value, storage_type storage, ttype type, symbol? type_declaration, abi abi, access access, presence presence, bool value_attr, bool target_attr, bool contiguous_attr, string? bindc_name, bool is_volatile, bool is_protected)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `Variable::m_parent_symtab` cannot be `nullptr`
* Variable '[...]' not found in parent_symtab symbol table
* Variable's parent symbol table does not point to it
* Variable's parent-symbolTable and actuall parent symbolTable don't match (Maybe inserted from another symbolTable)
* `Variable::m_parent_symtab` must be present in the ASR ([...])
* Initialisation of [...] must reduce to a compile time constant.
* Initialisation of [...] must reduce to a compile time constant.
* Variable symbol of string type can't have a length of kind `ImplicitLength`
* Variable of string type with length kind `DeferredLength` must be allocatable OR pointer
* Cbind character variable that isn't local must have length kind `ExpressionLength`
* Cbind character variable that isn't local must have length 1
* CChar-string-physical type shouldn't be used with local variables
* Variable [...] doesn't depend on [...] but is found in its dependency list.
* Variable [...] depends on [...] but isn't found in its dependency list.
* Variable [...] of type StructType must have a type declaration.
<!-- END AUTO: verify -->
