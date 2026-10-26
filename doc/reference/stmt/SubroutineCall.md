<!-- This is an automatically generated file. Do not edit it manually. -->

# SubroutineCall

SubroutineCall, a **stmt** node.

## Declaration

### Syntax

SubroutineCall(symbol name, symbol? original_name, call_arg* args, expr? dt)

### Arguments
Input arguments are `name` of type symbol, `original_name` of type symbol?, `args` of type call_arg*, `dt` of type expr?.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* SubroutineCall::m_name cannot point outside of its symbol table
* SubroutineCall::m_name is a Variable, but does not point to Function
* SubroutineCall::m_name is a Variable, but the type is not FunctionType
* SubroutineCall::m_name must be a Function or ClassProcedure.