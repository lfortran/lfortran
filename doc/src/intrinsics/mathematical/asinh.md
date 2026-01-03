# asinh(x): Inverse Hyperbolic Arcsine

Inverse hyperbolic arcsine function.

## Declaration

### Syntax

```fortran
retval = asinh(x)
elemental real(p) function asinh(x)
elemental complex(p) function asinh(x)
```
### Arguments

`x` the input value, can be real with value greater than or equal to 1 or of
type complex.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**asinh(x)** computes the inverse hyperbolic arcsine function of **x**.

The result type and kind are the same as input value `x`.

If the result is complex, the real part is non-negative, and the imaginary part
is expressed in radians and lies in the range

$\frac{-\pi}{2} <= aimag (asinh(x)) <= \frac{\pi}{2}$

## Types

Supported argument types float, double, complex float, complex double.

```fortran
interface asinh
    module procedure sasinh, dasinh, casinh, zasinh
end interface

contains

interface
    elemental real(sp) function sasinh(x)
    real(sp), intent(in) :: x
    end function

    elemental real(dp) function dasinh(x)
    real(dp), intent(in) :: x
    end function

    elemental complex(sp) function casinh(x)
    complex(sp), intent(in) :: x
    end function

    elemental complex(dp) function zasinh(x)
    complex(dp), intent(in) :: x
    end function
end interface
```
## Examples

```fortran
program intrinsics_asinh
    implicit none
    print *, asinh(1.0)
end program
```

**Result**:

```
-0.88137
```
## See Also

[acosh](acosh.md), [atanh](atanh.md).
