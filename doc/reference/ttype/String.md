# String

## ASR

<!-- BEGIN AUTO: asr -->
```
String(int kind, expr? len, string_length_kind len_kind, string_physical_type physical_type)
```
<!-- END AUTO: asr -->

## Documentation

The cases that this `String` type covers are:

* Assumed length dummy argument string in a subroutine:
  - `character(len=*), intent(in) :: x` → `String(1, (), AssumedLength, DescriptorString)`

* Deferred length string (can be a subroutine argument or a local variable):
  - `character(len=:), allocatable :: x` → `Allocatable(String(1, (), DeferredLength, DescriptorString))`

* Local variable or argument with runtime length:
  - `character(len=n) :: x` → `String(1, n, ExpressionLength, DescriptorString)`

* Local variable or argument with constant length:
  - `character(len=10) :: x` → `String(1, 10, ExpressionLength, FixedSizeString)`

* ISO C binding C string:
  - `character(len=n, kind=c_char) :: x` → `String(1, n, ExpressionLength, CString)`

### Physical Type Correspondence

The String physical types mirror arrays:

| Array Physical Type | String Physical Type |
|---------------------|---------------------|
| `FixedSizeArray` | `FixedSizeString` |
| `PointerToDataArray` | `PointerString` |
| `UnboundedPointerToDataArray` | `CString` |
| `DescriptorArray` | `DescriptorString` |

### Constraints

* Allocatable strings must be `DescriptorString`
* `PointerString` or `FixedSizeString` cannot be allocatable
* `PointerString` must have a length (non-null `len`), except when casting a descriptor string to a pointer string

### Ownership

* If the variable is declared in a local scope, it is owned by the local scope and must be deallocated when it goes out of scope
* If the variable is an argument, it is owned by the caller
* If the variable is allocatable (whether argument or local), it is treated just like an allocatable array

## Verify

<!-- BEGIN AUTO: verify -->
* String length must be of type INTEGER,found [...]
* String length must be length >= 0 Current length is -> [...]
* String of physical type [...] + existing length => must have length kind of `ExpressionLength`.
* String of physical type [...] + non-existing length => must have length kind of `AssumedLength` OR `DeferredLength` OR `ImplicitLength`.
* Implicit length kind must appear in StringPhysicalCast expression.
<!-- END AUTO: verify -->
