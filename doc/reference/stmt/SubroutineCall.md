# SubroutineCall

## ASR

<!-- BEGIN AUTO: asr -->
```
SubroutineCall(symbol name, symbol? original_name, call_arg* args, expr? dt, bool strict_bounds_checking)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `SubroutineCall::m_name` '[...]' cannot point outside of its symbol table
* `SubroutineCall::m_name` '[...]' is a Variable, but does not point to Function
* `SubroutineCall::m_name` '[...]' is a Variable, but the type is not FunctionType
* `SubroutineCall::m_name` '[...]' must be a Function or StructMethodDeclaration.
<!-- END AUTO: verify -->
