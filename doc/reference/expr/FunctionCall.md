# FunctionCall

## ASR

<!-- BEGIN AUTO: asr -->
```
FunctionCall(symbol name, symbol? original_name, call_arg* args, ttype type, expr? value, expr? dt)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `FunctionCall::m_name` must be present
* `FunctionCall::m_name` `[...]` cannot point outside of its symbol table
* `FunctionCall::m_name` must be a Function or Variable with FunctionType
* `FunctionCall::m_name` [...] must be returning a non-void value.
<!-- END AUTO: verify -->
