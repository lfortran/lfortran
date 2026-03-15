# DoConcurrentLoop

## ASR

<!-- BEGIN AUTO: asr -->
```
DoConcurrentLoop(do_loop_head* head, expr* shared, expr* local, reduction_expr* reduction, stmt* body)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `DoConcurrentLoop::m_local` must be a Var
* `DoConcurrentLoop::m_shared` must be a Var
<!-- END AUTO: verify -->
