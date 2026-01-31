# SubroutineCall

SubroutineCall, a **stmt** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
SubroutineCall(symbol name, symbol? original_name, call_arg* args, expr? dt, bool strict_bounds_checking)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `name` of type `symbol`, `original_name` of type `symbol?`, `args` of type `call_arg*`, `dt` of type `expr?`, `strict_bounds_checking` of type `bool`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* SubroutineCall::m_name '[...]' cannot point outside of its symbol table
* SubroutineCall::m_name '[...]' is a Variable, but does not point to Function
* SubroutineCall::m_name '[...]' is a Variable, but the type is not FunctionType
* SubroutineCall::m_name '[...]' must be a Function or StructMethodDeclaration.
<!-- END AUTO: restrictions -->
