# StringPhysicalCast

StringPhysicalCast, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
StringPhysicalCast(expr arg, string_physical_type old, string_physical_type new, ttype type, expr? value)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `arg` of type `expr`, `old` of type `string_physical_type`, `new` of type `string_physical_type`, `type` of type `ttype`, `value` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* x.`m_type` cannot be `nullptr`
* StringPhysicalCast should be of string type
* StringPhysicalCast return type shouldn't have length (Length should be implicit).
* StringPhysicalCast expression should have length kind of `ImplicitLength`.
<!-- END AUTO: restrictions -->
