# conjg(x): Complex Conjugate

Complex conjugate function.

## Declaration

### Syntax

```fortran
retval = conjg(x)
function conjgz32(x)
function conjgz64(x)
```

### Arguments

`x` the input value must be of type complex.

### Return values

The return value is of complex type.

## Description

**conjg(x)** converts the input value `x`  to its conjugate.

For `x` complex input value, the result is calculated using mathematical formula:

$x + iy$ converted to $x - iy$

$i$ the imaginary part is the result.

## Types

Supported argument types is complex.

```fortran
interface conjg
    module procedure conjgz32, conjgz64
end interface

contains

function conjgz32(x)
    complex(sp) :: x
end function

function conjgz64(x)
    complex(dp) :: x
end function
```

## Examples

```fortran
program intrinsics_conjg
    implicit none
	print *, conjg(1.0, -3.0)
end program
```

**Result:**

```
1 + 3i
```

## See Also
