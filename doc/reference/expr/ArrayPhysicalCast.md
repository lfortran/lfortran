# ArrayPhysicalCast

ArrayPhysicalCast, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
ArrayPhysicalCast(expr arg, array_physical_type old, array_physical_type new, ttype type, expr? value)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `arg` of type `expr`, `old` of type `array_physical_type`, `new` of type `array_physical_type`, `type` of type `ttype`, `value` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* ArrayPhysicalCast is redundant, the old physical type and new physical type must be different.
* Destination physical type conflicts with the physical type of target
* Old physical type conflicts with the physical type of argument [...] [...]
<!-- END AUTO: restrictions -->
