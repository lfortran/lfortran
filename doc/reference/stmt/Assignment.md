# Assignment

## ASR

<!-- BEGIN AUTO: asr -->
```
Assignment(expr target, expr value, stmt? overloaded, bool realloc_lhs, bool move_allocation)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* Assignment target `[...]` with intent `IN` not allowed
* Assignment target with [...] cannot be re-assigned.
* Reallocation of non allocatable variable is not allowed
* Move assignment target must be an allocatable array
* Move assignment value must be an allocatable array
<!-- END AUTO: verify -->
