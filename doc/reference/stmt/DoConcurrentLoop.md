# DoConcurrentLoop

DoConcurrentLoop, a **stmt** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
DoConcurrentLoop(do_loop_head* head, expr* shared, expr* local, reduction_expr* reduction, stmt* body)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `head` of type `do_loop_head*`, `shared` of type `expr*`, `local` of type `expr*`, `reduction` of type `reduction_expr*`, `body` of type `stmt*`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* `DoConcurrentLoop::m_local` must be a Var
* `DoConcurrentLoop::m_shared` must be a Var
<!-- END AUTO: restrictions -->
