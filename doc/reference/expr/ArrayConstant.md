<!-- This is an automatically generated file. Do not edit it manually. -->

# ArrayConstant

ArrayConstant, a **expr** node.

## Declaration

### Syntax

ArrayConstant(int n_data, void data, ttype type, arraystorage storage_format)

### Arguments
Input arguments are `n_data` of type int, `data` of type void, `type` of type ttype, `storage_format` of type arraystorage.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Type of ArrayConstant must be an array
* Constant array of strings should have constant string length
* ArrayConstant::m_n_data must match the byte size of the array