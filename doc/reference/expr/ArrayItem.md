<!-- This is an automatically generated file. Do not edit it manually. -->

# ArrayItem

ArrayItem, a **expr** node.

## Declaration

### Syntax

ArrayItem(expr v, array_index* args, ttype type, arraystorage storage_format, expr? value)

### Arguments
Input arguments are `v` of type expr, `args` of type array_index*, `type` of type ttype, `storage_format` of type arraystorage, `value` of type expr?.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* ArrayItem::m_type with array indices must be an array.ArrayItem::m_type cannot be array.