# Struct

Struct, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Struct(symbol_table symtab, identifier name, ttype struct_signature, identifier* dependencies, identifier* members, identifier* member_functions, abi abi, access access, bool is_packed, bool is_abstract, call_arg* initializers, expr? alignment, symbol? parent)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `symtab` of type `symbol_table`, `name` of type `identifier`, `struct_signature` of type `ttype`, `dependencies` of type `identifier*`, `members` of type `identifier*`, `member_functions` of type `identifier*`, `abi` of type `abi`, `access` of type `access`, `is_packed` of type `bool`, `is_abstract` of type `bool`, `initializers` of type `call_arg*`, `alignment` of type `expr?`, `parent` of type `symbol?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Alignment [...] is not a positive power of 2.
<!-- END AUTO: restrictions -->
