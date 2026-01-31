<!-- This is an automatically generated file. Do not edit it manually. -->
# StructMethodDeclaration

StructMethodDeclaration, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
StructMethodDeclaration(symbol_table parent_symtab, identifier name, identifier? self_argument, identifier proc_name, symbol proc, abi abi, bool is_deferred, bool is_nopass)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `parent_symtab` of type `symbol_table`, `name` of type `identifier`, `self_argument` of type `identifier?`, `proc_name` of type `identifier`, `proc` of type `symbol`, `abi` of type `abi`, `is_deferred` of type `bool`, `is_nopass` of type `bool`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* The `StructMethodDeclaration::m_name` cannot be `nullptr`
* The `StructMethodDeclaration::m_proc` cannot be `nullptr`
* The `StructMethodDeclaration::m_proc_name` cannot be `nullptr`
* `StructMethodDeclaration::m_parent_symtab` cannot be `nullptr`
* StructMethodDeclaration '[...]' not found in parent_symtab symbol table
* StructMethodDeclaration's parent symbol table does not point to it
* `StructMethodDeclaration::m_parent_symtab` must be present in the ASR ([...])
* must be present in [...] procedures.
<!-- END AUTO: restrictions -->
