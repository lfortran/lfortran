# ArrayConstant

ArrayConstant, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
ArrayConstant(int n_data, void data, ttype type, arraystorage storage_format)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `n_data` of type `int`, `data` of type `void`, `type` of type `ttype`, `storage_format` of type `arraystorage`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Type of ArrayConstant must be an array
* Constant array of strings should have constant string length
* `ArrayConstant::m_n_data` must match the byte size of the array
<!-- END AUTO: restrictions -->
