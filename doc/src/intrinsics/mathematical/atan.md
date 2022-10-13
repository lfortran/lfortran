# atan(x): Trigonometric ArcTangent

Trigonometric arctangent (inverse arctangent) function.

## Declaration

### Syntax

```fortran
retval = atan(x)
elemental real(p) function atan(x)
elemental complex(p) function atan(x)
```
### Arguments

`x` the input value, can be real or complex; less than or equal to 1.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**atan(x)** computes the arctangent of the argument **x**.

The arc tangent is the inverse function of the arctangent function. It is commonly used in
trigonometry to find the angle when the lengths of the opposite side of a right triangle
and base are known, i.e., perpendicular and base length.

## Types

Supported argument types float, double, complex float, complex double.

```fortran
interface atan
    module procedure satan, datan, catan, zatan
end interface

contains

interface
    elemental real(sp) function satan(x)
    real(sp), intent(in) :: x
    end function

    elemental real(dp) function datan(x)
    real(dp), intent(in) :: x
    end function

    elemental complex(sp) function catan(x)
    complex(sp), intent(in) :: x
    end function

    elemental complex(dp) function zatan(x)
    complex(dp), intent(in) :: x
    end function
end interface
```
## Examples

```fortran
program intrinsics_atan
implicit none
integer, parameter :: arg_x = kind(0.0)
real(arg_x) :: x1
real :: retval_x
real :: arg_x
retval_x = atan(0.84147098)
print *, ret_val_x
retval_x = atan(x1)
print *, ret_val_x
end program
```

**Result**:

```
0.699521642
0.699521642
```
## See Also

[asin](asin.md), [acos](acos.md).
