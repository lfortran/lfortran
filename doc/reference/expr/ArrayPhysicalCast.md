<!-- This is an automatically generated file. Do not edit it manually. -->

# ArrayPhysicalCast

ArrayPhysicalCast, a **expr** node.

## Declaration

### Syntax

ArrayPhysicalCast(expr arg, array_physical_type old, array_physical_type new, ttype type, expr? value)

### Arguments
Input arguments are `arg` of type expr, `old` of type array_physical_type, `new` of type array_physical_type, `type` of type ttype, `value` of type expr?.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* ArrayPhysicalCast is redundant,the old physical type and new physical type must be different.
* Destination physical type conflicts with the physical type of target
* Old physical type conflicts with the physical type of argument