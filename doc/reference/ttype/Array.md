# Array

Array, a **ttype** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Array(ttype type, dimension* dims, array_physical_type physical_type)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `type` of type `ttype`, `dims` of type `dimension*`, `physical_type` of type `array_physical_type`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Allocatable cannot be inside array
* Assumed-rank arrays must have 0 dimensions
* Array type cannot have 0 dimensions.
* Array type cannot be nested.
* Array of strings' physical type shouldn't be "FixedSizeArray"
* Array of classes can't be of physical type "FixedSizeArray"
<!-- END AUTO: restrictions -->
