# floor(x, kind): Integer Floor

Integer floor function.

## Declaration

### Syntax

```fortran
retval = floor(x)
elemental integer function floor(x, kind)
```

### Arguments

`x` the input value must be of type real.

`kind` the optional input parameter must be a scalar integer constant expression.

### Return values

The return value is of type integer(kind) if kind is passed as input parameter.
If not, default kind integer is returned.

The return value is equal to or nearest greatest integer less than or equal to
`x`.

## Description

**floor(x)** returns the greatest integer less than or equal to x. It returns
an integer value unless spefically specified using second optional paramter.

## Types

Supported argument types is real.

```fortran
interface floor
    module procedure sfloor_i32, sfloor_i64, dfloor_i32, dfloor_i64
end interface

contains

elemental integer(i32) function sfloor_i32(x, kind)
real(sp), intent(in) :: x
integer(i32), intent(in) :: kind
end function

elemental integer(i64) function sfloor_i64(x, kind)
real(sp), intent(in) :: x
integer(i64), intent(in) :: kind
end function

elemental integer(i32) function dfloor_i32(x, kind)
real(dp), intent(in) :: x
integer(i32), intent(in) :: kind
end function

elemental integer(i64) function dfloor_i64(x, kind)
real(dp), intent(in) :: x
integer(i64), intent(in) :: kind
end function
```

## Examples

```fortran
program intrinsics_floor
    implicit none
	real, parameter :: x = 3.1
	print *, floor(x)
	print *, floor(-3.1)
end program
```

**Result:**

```
3
-4
```

## See Also

[ceiling](ceiling.md), [mod](mod.md).
