<!-- This is an automatically generated file. Do not edit it manually. -->

# DoConcurrentLoop

DoConcurrentLoop, a **stmt** node.

## Declaration

### Syntax

DoConcurrentLoop(do_loop_head* head, expr* shared, expr* local, reduction_expr* reduction, stmt* body)

### Arguments
Input arguments are `head` of type do_loop_head*, `shared` of type expr*, `local` of type expr*, `reduction` of type reduction_expr*, `body` of type stmt*.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* DoConcurrentLoop::m_local must be a Var
* DoConcurrentLoop::m_shared must be a Var