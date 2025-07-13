<!-- This is an automatically generated file. Do not edit it manually. -->

# Allocate

Allocate, a **stmt** node.

## Declaration

### Syntax

Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)

### Arguments
Input arguments are `args` of type alloc_arg*, `stat` of type expr?, `errmsg` of type expr?, `source` of type expr?.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Allocate should only be called with Allocatable or Pointer type inputs, found
* Allocate::m_sym_subclass must point to a Struct_t when the m_a member is of a type StructType
* Allocate for arrays should have dimensions specified,found only array variable with no dimensions