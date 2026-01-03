# acosh(x): Inverse Hyperbolic Cosine

Inverse hyperbolic cosine function.

## Declaration

### Syntax

```fortran
retval = acosh(x)
elemental real(p) function acosh(x)
elemental complex(p) function acosh(x)
```
### Arguments

`x` the input value, can be real with value greater than or equal to 1 or of
type complex.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**acosh(x)** computes the inverse hyperbolic cosine function of **x**.

The result type and kind are the same as input value `x`.

If the result is complex, the real part is non-negative, and the imaginary part
is expressed in radians and lients in the range

$-\pi <= img (acosh(x)) <= \pi$

For real values $x$ in the domain $x > 1$, the inverse hyperbolic cosine
satisifies:

$cosh^{-1}(x) = \log(x + \sqrt{(x^2 - 1)})$

For complex numbers $x = x + iy$, as well as real values in the domain
$-\infty < z <= 1$, the call $acosh(z)$ returns complex results.

## Types

Supported argument types float, double, complex float, complex double.

```fortran
interface acosh
    module procedure sacosh, dacosh, cacosh, zacosh
end interface

contains

interface
    elemental real(sp) function sacosh(x)
    real(sp), intent(in) :: x
    end function

    elemental real(dp) function dacosh(x)
    real(dp), intent(in) :: x
    end function

    elemental complex(sp) function cacosh(x)
    complex(sp), intent(in) :: x
    end function

    elemental complex(dp) function zacosh(x)
    complex(dp), intent(in) :: x
    end function
end interface
```
## Examples

```fortran
program intrinsics_acosh
implicit none
print *, acosh(1.0)
end program
```

**Result**:

```
0.0
```
## See Also

[asinh](asinh.md), [atanh](atanh.md).
