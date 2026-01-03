# atanh(x): Inverse Hyperbolic Tangent

Inverse hyperbolic tangent function.

## Declaration

### Syntax

```fortran
retval = atanh(x)
elemental real(p) function atanh(x)
elemental complex(p) function atanh(x)
```
### Arguments

`x` the input value, can be real with value greater than or equal to 1 or of
type complex.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**atanh(x)** computes the inverse hyperbolic tangent function of **x**.

The result type and kind are the same as input value `x`.

If the result is complex, the imaginary part is expressed in radians and lies
in the range

$\frac{-\pi}{2} <= aimag (atanh(x)) <= \frac{\pi}{2}$

If the result is real, it lies in the range

$-1.0 < atanh(x) < 1.0$

## Types

Supported argument types float, double, complex float, complex double.

```fortran
interface atanh
    module procedure satanh, datanh, catanh, zatanh
end interface

contains

interface
    elemental real(sp) function satanh(x)
    real(sp), intent(in) :: x
    end function

    elemental real(dp) function datanh(x)
    real(dp), intent(in) :: x
    end function

    elemental complex(sp) function catanh(x)
    complex(sp), intent(in) :: x
    end function

    elemental complex(dp) function zatanh(x)
    complex(dp), intent(in) :: x
    end function
end interface
```
## Examples

```fortran
program intrinsics_atanh
    implicit none
    print *, atanh(0.0)
end program
```

**Result**:

```
0.0
```
## See Also

[asinh](asinh.md), [acosh](acosh.md).
