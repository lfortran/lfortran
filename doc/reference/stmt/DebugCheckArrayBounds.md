# DebugCheckArrayBounds

## ASR

<!-- BEGIN AUTO: asr -->
```
DebugCheckArrayBounds(expr target, expr* components, bool move_allocation)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `DebugCheckArrayBounds::m_target` must have an Array type
* `DebugCheckArrayBounds::n_components` should be greater than 0
* `DebugCheckArrayBounds::m_components` element must be Var, ArrayPhysicalCast, StructInstanceMember, BitCast, or ArrayConstant
* `DebugCheckArrayBounds::m_components` element must have an Array type
<!-- END AUTO: verify -->
