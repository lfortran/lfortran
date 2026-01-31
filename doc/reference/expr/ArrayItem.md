# ArrayItem

ArrayItem, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
ArrayItem(expr v, array_index* args, ttype type, arraystorage storage_format, expr? value)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `v` of type `expr`, `args` of type `array_index*`, `type` of type `ttype`, `storage_format` of type `arraystorage`, `value` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* ArrayItem::m_type with array indices must be an array.
* ArrayItem::m_type cannot be array.
<!-- END AUTO: restrictions -->
