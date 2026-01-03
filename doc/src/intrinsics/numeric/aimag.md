# aimag(x): Imaginary Part

Imaginary part of complex number.

## Declaration

### Syntax

```fortran
retval = aimag(x)
elemental real(sp) function aimag(x)
elemental real(dp) function aimag(x)
```

### Arguments

`x` the input value must be of complex type.

### Return values

The return value is imaginary part of type real and kind same as of `x`.

## Description

**aimag(x)** returns the imaginary part of the input complex argument `x`.

For `x` complex input value, the result is calculated using mathematical formula:

$x + iy$

$i$ the imaginary part is the result.

## Types

Supported argument types are complex.

```fortran
interface aimag
    module procedure saimag, daimag
end interface

contains

elemental real(sp) function saimag(x)
complex(sp), intent(in) :: x
end function

elemental real(dp) function daimag(x)
complex(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_aimag
    implicit none
	print *, aimag(1.0, -3.0)
end program
```

**Result:**

```
-3.0
```

## See Also

[ceiling](ceiling.md), [floor](floor.md).
