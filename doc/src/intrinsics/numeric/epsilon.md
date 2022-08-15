# epsilon(x): Epsilon

Epsilon function.

## Declaration

### Syntax

```fortran
retval = epsilon(x)
elemental real function epsilon(x)
```

### Arguments

`x` the input value must be of type real.

### Return values

The return value is of same type as of the input argument.

## Description

**epsilon(x)** computes the smallest number $\epsilon$ of the same kind as `x`
following:

$1 + \epsilon > 1$

## Types

Supported argument types is real.

```fortran
interface epsilon
    module procedure sepsilon, depsilon
end interface

contains

elemental real(sp) function sepsilon(x)
real(sp), intent(in) :: x
end function

elemental real(dp) function depsilon(x)
real(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_epsilon
	print *, epsilon(3.143)
end program
```

**Result:**

```
1.19209290E-07
```

## See Also
