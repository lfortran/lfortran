# acos(x): Trigonometric Arc Cosine

Trigonometric arc cosine (inverse cosine) function.

## Declaration

### Syntax

```fortran
retval = acos(x)
elemental real(p) function acos(x)
elemental complex(p) function acos(x)
```
### Arguments

`x` the input value, can be real or complex; less than or equal to 1.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**acos(x)** computes the arcsine of the argument **x**.

The arc cosine is the inverse function of the cosine function. It is commonly used in
trigonometry to find the angle when the lengths of the hypotenuse and the
base side  of a right triangle are known.

## Types

Supported argument types float, double, complex float, complex double.

```fortran
interface acos
    module procedure sacos, dacos, cacos, zacos
end interface

contains

interface
    elemental real(sp) function sacos(x)
    real(sp), intent(in) :: x
    end function

    elemental real(dp) function dacos(x)
    real(dp), intent(in) :: x
    end function

    elemental complex(sp) function cacos(x)
    complex(sp), intent(in) :: x
    end function

    elemental complex(dp) function zacos(x)
    complex(dp), intent(in) :: x
    end function
end interface
```
## Examples

```fortran
program intrinsics_acos
implicit none
integer, parameter :: arg_x = kind(0.0)
real(arg_x) :: x1
real :: retval_x
real :: arg_x
retval_x = acos(0.84147098)
print *, ret_val_x
retval_x = acos(x1)
print *, ret_val_x
end program
```

**Result**:

```
0.570796336
0.570796336
```
## See Also

[asin](asin.md), [atan](atan.md).
