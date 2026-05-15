# StructMethodDeclaration

## ASR

<!-- BEGIN AUTO: asr -->
```
StructMethodDeclaration(symbol_table parent_symtab, identifier name, identifier? self_argument, identifier proc_name, symbol proc, abi abi, bool is_deferred, bool is_nopass)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `StructMethodDeclaration::m_name` cannot be `nullptr`
* The `StructMethodDeclaration::m_proc` cannot be `nullptr`
* The `StructMethodDeclaration::m_proc_name` cannot be `nullptr`
* `StructMethodDeclaration::m_parent_symtab` cannot be `nullptr`
* StructMethodDeclaration '[...]' not found in parent_symtab symbol table
* StructMethodDeclaration's parent symbol table does not point to it
* `StructMethodDeclaration::m_parent_symtab` must be present in the ASR ([...])
* must be present in [...] procedures.
<!-- END AUTO: verify -->
