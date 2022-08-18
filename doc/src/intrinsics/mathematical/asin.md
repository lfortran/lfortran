# asin(x): Trigonometric Arcsine

Trigonometric arcsine function.

## Declaration

### Syntax

```fortran
retval = asin(x)
elemental real(p) function asin(x)
elemental complex(p) function asin(x)
```
### Arguments

`x` the input value, can be real or complex; less than or equal to 1.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**asin(x)** computes the arcsine of the argument **x**.

The arcsine is the inverse function of the sine function. It is commonly used in
trigonometry to find the angle when the lengths of the hypotenuse and the
opposite side of a right triangle are known.

## Types

Supported argument types float, double, complex float, complex double.

```fortran
interface asin
    module procedure sasin, dasin, casin, zasin
end interface

contains

interface
    elemental real(sp) function sasin(x)
    real(sp), intent(in) :: x
    end function

    elemental real(dp) function dasin(x)
    real(dp), intent(in) :: x
    end function

    elemental complex(sp) function casin(x)
    complex(sp), intent(in) :: x
    end function

    elemental complex(dp) function zasin(x)
    complex(dp), intent(in) :: x
    end function
end interface
```
## Examples

```fortran
program intrinsics_asin
    implicit none
    integer, parameter :: arg_x = kind(0.0)
    real(arg_x) :: x1
    real :: retval_x
    real :: arg_x
    retval_x = asin(0.84147098)
    print *, ret_val_x
    retval_x = asin(x1)
    print *, ret_val_x
end program
```

**Result**:

```
0.99999
0.99999
```
## See Also

[acos](acos.md), [atan](atan.md).
