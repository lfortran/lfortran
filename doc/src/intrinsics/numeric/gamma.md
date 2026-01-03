# gamma(x): Gamma

Gamma function.

## Declaration

### Syntax

```fortran
retval = gamma(x)
elemental real function gamma(x)
```

### Arguments

`x` the input value must be of type real. It should not be zero or a negative
integer.

### Return values

The return value is of same type and kind as of `x`.

## Description

**gamma(x)** computes $\gamma(x)$. For positive, integer value of `x`, the Gamma
function simplifies to factorial function:

$\gamma(x) = (x-1)!$

In general, if $x > 0$:

$\gamma(x) = \int_{0}^{\infty} e^{-t} dt$

and if $-n-1 < x < -n$ where n is an integer >= 0:

$\gamma(x) = \int_{0}^{\infty}(e ^{-t} - \sum\limits_{k=0}^n \frac{(-t)^k}{k!} dt)$

## Types

Supported argument types is real.

```fortran
interface gamma
    module procedure sgamma, dgamma
end interface

contains

elemental real(sp) function sgamma(x)
real(sp), intent(in) :: x
end function

elemental real(dp) function dgamma(x)
real(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_gamma
	print *, gamma(0.5)
	print *, gamma(1.0)
end program
```

**Result:**

```
1.77245
1.000
```

## See Also

[epsilon](epsilon.md)
