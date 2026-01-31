# FunctionCall

FunctionCall, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
FunctionCall(symbol name, symbol? original_name, call_arg* args, ttype type, expr? value, expr? dt)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `name` of type `symbol`, `original_name` of type `symbol?`, `args` of type `call_arg*`, `type` of type `ttype`, `value` of type `expr?`, `dt` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* FunctionCall::m_name must be present
* FunctionCall::m_name `[...]` cannot point outside of its symbol table
* FunctionCall::m_name must be a Function or Variable with FunctionType
* FunctionCall::m_name [...] must be returning a non-void value.
<!-- END AUTO: restrictions -->
