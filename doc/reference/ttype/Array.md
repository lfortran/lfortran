# Array

## ASR

<!-- BEGIN AUTO: asr -->
```
Array(ttype type, dimension* dims, array_physical_type physical_type, memory_space memory_space)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* Allocatable cannot be inside array
* Assumed-rank arrays must have 0 dimensions
* Array type cannot have 0 dimensions.
* Array type cannot be nested.
* Array of strings' physical type shouldn't be `FixedSizeArray`
* Array of strings with physical type `StringArraySinglePointer` must have string physical type `CChar`, not `DescriptorString`
* Array of classes can't be of physical type `FixedSizeArray`
<!-- END AUTO: verify -->
