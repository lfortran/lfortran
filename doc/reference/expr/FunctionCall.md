<!-- This is an automatically generated file. Do not edit it manually. -->

# FunctionCall

FunctionCall, a **expr** node.

## Declaration

### Syntax

FunctionCall(symbol name, symbol? original_name, call_arg* args, ttype type, expr? value, expr? dt)

### Arguments
Input arguments are `name` of type symbol, `original_name` of type symbol?, `args` of type call_arg*, `type` of type ttype, `value` of type expr?, `dt` of type expr?.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* FunctionCall::m_name must be present
* FunctionCall::m_name `` cannot point outside of its symbol table
* FunctionCall::m_name must be a Function or Variable with FunctionType
* FunctionCall::m_namemust be returning a non-void value.