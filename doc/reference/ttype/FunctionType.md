# FunctionType

FunctionType, a **ttype** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
FunctionType(ttype* arg_types, ttype? return_var_type, abi abi, deftype deftype, string? bindc_name, bool elemental, bool pure, bool module, bool inline, bool static, symbol* restrictions, bool is_restriction)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `arg_types` of type `ttype*`, `return_var_type` of type `ttype?`, `abi` of type `abi`, `deftype` of type `deftype`, `bindc_name` of type `string?`, `elemental` of type `bool`, `pure` of type `bool`, `module` of type `bool`, `inline` of type `bool`, `static` of type `bool`, `restrictions` of type `symbol*`, `is_restriction` of type `bool`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* ASR::ttype_t in ASR::FunctionType cannot be tied to a scope.
<!-- END AUTO: restrictions -->
