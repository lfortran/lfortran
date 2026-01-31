<!-- This is an automatically generated file. Do not edit it manually. -->
# ArrayConstructor

ArrayConstructor, a **expression (expr)** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
ArrayConstructor(expr* args, ttype type, expr? value, arraystorage storage_format, expr? struct_var)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `args` of type `expr*`, `type` of type `ttype`, `value` of type `expr?`, `storage_format` of type `arraystorage`, `struct_var` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Type of ArrayConstructor must be an array
* `ArrayConstructor::m_struct_vars` must be `nullptr` or var to struct symbol
<!-- END AUTO: restrictions -->
