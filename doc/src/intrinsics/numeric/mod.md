# mod(x, y): Modulus

Modulus or remainder function.

## Declaration

### Syntax

```fortran
retval = mod(x, y)
elemental integer function imod(x, y)
elemental real(sp) function mod(x, y)
```

### Arguments

`x` the input value, can be integer or real.

`y` second input parameter should be same type and kind as `x` and not equal to
0.

### Return values

The return value is of type and kind same as of `x`. The result is calculated
using mathematical modulo or remainder as:

`x - INT (x / y) * y`.

If `y` is 0, the result is undefined like `any integer value / 0`.

## Description

**mod(x, y)** calculates and returns modulo or remainder when `x` is divided
by `y`. The result is calculated using

`x - INT(x / y) * y`

i.e., result is remainder when first argument is mathematically divided by
second argument.

The return value should be less than `y`.

## Types

Supported argument types are real and integer.

```fortran
interface mod
    module procedure i8mod, i16mod, imod, i64mod, smod, dmod
end interface

contains

elemental integer(i16) function i16mod(x, y)
integer(i16), intent(in) :: x, y
end function

elemental integer(i8) function i8mod(x, y)
integer(i8), intent(in) :: x, y
end function

elemental integer function imod(x, y)
integer, intent(in) :: x, y
end function

elemental integer function i64mod(x, y)
integer(i64), intent(in) :: x, y
end function

elemental real(sp) function smod(x, y)
real(sp), intent(in) :: x, y
end function

elemental real(dp) function dmod(x, y)
real(dp), intent(in) :: x, y
end function

```

## Examples

```fortran
program intrinsics_mod
    implicit none
	print *, mod(-5, 3)
end program
```

**Result:**

```
-2
```

## See Also

[ceiling](ceiling.md), [floor](floor.md).
