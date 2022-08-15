# exp(x): Exponential

Exponential function.

## Declaration

### Syntax

```fortran
retval = exp(x)
elemental real function exp(x)
```

### Arguments

`x` the input value must be of type real or complex.

### Return values

The return value is of same type as of the input argument, `x`.

## Description

**exp(x)** computes the base $e$ exponential of `x`, i.e., $e^x$.

If `x` is of type complex, its imaginary part is considered as a value in
radians.

## Types

Supported argument types is real and complex.

```fortran
interface exp
    module procedure sexp, dexp, cexp, zexp
end interface

contains

elemental real(sp) function sexp(x)
real(sp), intent(in) :: x
end function

elemental real(dp) function dexp(x)
real(dp), intent(in) :: x
end function

elemental complex(sp) function cexp(x)
complex(sp), intent(in) :: x
end function

elemental complex(dp) function zexp(x)
complex(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_exp
	print *, exp(0.5)
end program
```

**Result:**

```
1.64872
```

## See Also
