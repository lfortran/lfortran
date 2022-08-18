# erfc(x): Complementary Error

Complementary error function.

## Declaration

### Syntax

```fortran
retval = erfc(x)
elemental real function erfc(x)
```

### Arguments

`x` the input value must be of type real.

### Return values

The return value is of type real and of the same kind as of the input parameter
`x`. `erfc(x)` returns $1 - erf(x)$.

## Description

**erf(x)** computes the error function of `x`. The result lies in the range:

$0 \leq erf (x) \leq 2$

It is calculated using:

$\frac{2}{\sqrt\pi}\int_{x}^{\infty}e^{-t^2}dt$

It is used in instances when the large loss of relative accuracy occurs if
`erf(x)` is called for large `x` and the result is subtracted from 1.

## Types

Supported argument types is real.

```fortran
interface erfc
    module procedure serfc, derfc
end interface

contains

elemental real(sp) function serfc(x)
real(sp), intent(in) :: x
end function

elemental real(dp) function derfc(x)
real(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_erfc
	print *, erfc(1.0)
end program
```

**Result:**

```
0.1572992057
```

## See Also

[erf](erf.md).
