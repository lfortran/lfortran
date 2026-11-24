<!-- This is an automatically generated file. Do not edit it manually. -->

# Select

Select, a **stmt** node.

## Declaration

### Syntax

Select(identifier? name, expr test, case_stmt* body, stmt* default, bool enable_fall_through)

### Arguments
Input arguments are `name` of type identifier?, `test` of type expr, `body` of type case_stmt*, `default` of type stmt*, `enable_fall_through` of type bool.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Select_t::m_enable_fall_through should be