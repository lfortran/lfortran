# abs(x): Absolute Value

Absolute value.

## Declaration

### Syntax

```fortran
retval = abs(x)
elemental integer function iabs(x)
elemental real(sp) function abs(x)
```

### Arguments

`x` the input value, can be integer, real, or complex.

### Return values

The return value is of type and kind same as of `x`. For complex input value,
return value is real.

## Description

**abs(x)** calculates and returns absolute value of `x`. Result is calculated
using mathematical formula:

$|x|$

If `x` is complex, the result is calculated using mathematical formula:

$\sqrt{(x^2 + y^2)}$

## Types

Supported argument types are real and integer.

```fortran
interface mod
    module procedure iabs, i8abs, i16abs, i64abs, sabs, dabs, cabs, zabs
end interface

contains

elemental integer(i16) function i16abs(x)
integer(i16), intent(in) :: x
end function

elemental integer(i8) function i8abs(x)
integer(i8), intent(in) :: x
end function

elemental integer function iabs(x)
integer, intent(in) :: x
end function

elemental integer function i64abs(x)
integer(i64), intent(in) :: x
end function

elemental real(sp) function sabs(x)
real(sp), intent(in) :: x
end function

elemental real(dp) function dabs(x)
real(dp), intent(in) :: x
end function

elemental real(sp) function cabs(x)
complex(sp), intent(in) :: x
end function

elemental real(dp) function zabs(x)
complex(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_abs
    implicit none
	print *, abs(1.5)
	print *, abs(-1.5)
end program
```

**Result:**

```
1.5
1.5
```

## See Also

[ceiling](ceiling.md), [floor](floor.md).
