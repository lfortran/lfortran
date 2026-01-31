<!-- This is an automatically generated file. Do not edit it manually. -->
# DebugCheckArrayBounds

DebugCheckArrayBounds, a **statement (stmt)** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
DebugCheckArrayBounds(expr target, expr* components, bool move_allocation)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `target` of type `expr`, `components` of type `expr*`, `move_allocation` of type `bool`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* `DebugCheckArrayBounds::m_target` must have an Array type
* `DebugCheckArrayBounds::n_components` should be greater than 0
* `DebugCheckArrayBounds::m_components` element must be Var, ArrayPhysicalCast, StructInstanceMember, BitCast, or ArrayConstant
* `DebugCheckArrayBounds::m_components` element must have an Array type
<!-- END AUTO: restrictions -->
