<!-- This is an automatically generated file. Do not edit it manually. -->

# Assignment

Assignment, a **stmt** node.

## Declaration

### Syntax

Assignment(expr target, expr value, stmt? overloaded, bool realloc_lhs)

### Arguments
Input arguments are `target` of type expr, `value` of type expr, `overloaded` of type stmt?, `realloc_lhs` of type bool.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Assignment target `` with intent `IN` not allowed
* Assignment target withcannot be re-assigned.
* Reallocation of non allocatable variable is not allowed