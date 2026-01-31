<!-- This is an automatically generated file. Do not edit it manually. -->
# Assignment

Assignment, a **statement (stmt)** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Assignment(expr target, expr value, stmt? overloaded, bool realloc_lhs, bool move_allocation)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `target` of type `expr`, `value` of type `expr`, `overloaded` of type `stmt?`, `realloc_lhs` of type `bool`, `move_allocation` of type `bool`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Assignment target `[...]` with intent `IN` not allowed
* Assignment target with [...] cannot be re-assigned.
* Reallocation of non allocatable variable is not allowed
* Move assignment target must be an allocatable array
* Move assignment value must be an allocatable array
<!-- END AUTO: restrictions -->
