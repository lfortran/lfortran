# atan2(y, x): ArcTangent 

Arctangent function or inverse tangent function.

## Declaration

### Syntax

```fortran
retval = atan2(y, x)
elemental real(p) function atan2(y, x)
```
### Arguments

`y` the input value is the imaginary part of the complex expression

$x + iy$

`x` the input value, must be real part of the complex expression.

### Return values

The returned value has the kind and TYPE as of the input value `y`. The
principal value of the argument function of the complex expression $x + iy$
is returned.

## Description

**atan2(y, x)** computes the principal value of the argument function of the
complex expression $x + iy$. This is used to transform from cartesian into
polar coordinates and allows to determine the angle in the correct quadrant.

It is also represented as:

$tan^{-1}(\frac{y}{x})$

If `x` is nonzero, the result lies in the range:

$-\pi <= atan(x) <= \pi$

The sign is positive if `y` is positive.

If `y` is zero and `x` is strictly positive, then the result is 0.

If `x` is negative and `y` is positive zero, then the result is $\pi$.

If `x` is negative and `y` is negative zero, then the result is $-\pi$.

If `x` is zero, then the magnitude of the result is $\frac{\pi}{2}$.

If `y` is negative real zero, the result is $\frac{-\pi}{2}$.

## Types

Supported argument type is real.

```fortran
interface atan2
    module procedure satan2, datan2
end interface

contains

interface
    elemental real(sp) function satan2(y, x)
    real(sp), intent(in) :: y, x
    end function

    elemental real(dp) function datan2(y, x)
    real(dp), intent(in) :: y, x
    end function

end interface
```
## Examples

```fortran
program intrinsics_atan2
    implicit none
    print *, atan2(2.679676, 1.0)
end program
```

**Result**:

```
0.699521642
```

## See Also

