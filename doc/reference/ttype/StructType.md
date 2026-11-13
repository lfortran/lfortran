<!-- This is an automatically generated file. Do not edit it manually. -->

# StructType

StructType, a **ttype** node.

## Declaration

### Syntax

StructType(ttype* data_member_types, ttype* member_function_types, bool is_cstruct, symbol derived_type)

### Arguments
Input arguments are `data_member_types` of type ttype*, `member_function_types` of type ttype*, `is_cstruct` of type bool, `derived_type` of type symbol.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* StructType::m_derived_type cannot point outside of its symbol table, owner: