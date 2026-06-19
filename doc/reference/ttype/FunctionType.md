<!-- This is an automatically generated file. Do not edit it manually. -->

# FunctionType

FunctionType, a **ttype** node.

## Declaration

### Syntax

FunctionType(ttype* arg_types, ttype? return_var_type, abi abi, deftype deftype, string? bindc_name, bool elemental, bool pure, bool module, bool inline, bool static, symbol* restrictions, bool is_restriction)

### Arguments
Input arguments are `arg_types` of type ttype*, `return_var_type` of type ttype?, `abi` of type abi, `deftype` of type deftype, `bindc_name` of type string?, `elemental` of type bool, `pure` of type bool, `module` of type bool, `inline` of type bool, `static` of type bool, `restrictions` of type symbol*, `is_restriction` of type bool.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* ASR::ttype_t in ASR::FunctionTypecannot be tied to a scope.