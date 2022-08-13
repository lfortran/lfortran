# erf(x): Error

Error function.

## Declaration

### Syntax

```fortran
retval = erf(x)
elemental real function erf(x)
```

### Arguments

`x` the input value must be of type real.

### Return values

The return value is of type real and of the same kind as of the input parameter
`x`.

## Description

**erf(x)** computes the error function of `x`. The result lies in the range:

$-1 \leq erf (x) \leq 1$

It is calculated using:

$\frac{2}{\sqrt\pi}\int_0^xe^{-t^2}dt$

## Types

Supported argument types is real.

```fortran
interface erf
    module procedure serf, derf
end interface

contains

elemental real(sp) function serf(x)
real(sp), intent(in) :: x
end function

elemental real(dp) function derf(x)
real(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_erf
	print *, erf(0.5)
end program
```

**Result:**

```
0.520499877
```

## See Also

[erfc](erfc.md).
