# fraction(x): Fractional Part in Model Representation

Fractional part of the model representation.

## Declaration

### Syntax

```fortran
retval = fraction(x)
elemental real(p) function fraction(x)
```
### Arguments

`x` the input value, must be real.

### Return values

The returned value has the kind of the input value. The fractional part of the
model representation of the input value is returned.

## Description

**fraction(x)** computes the fractional part of the model representation of **x**.

It is calculated using:

$x * {radix(x)}^{(-exponent(x))}$

## Types

Supported argument type is real.

## Examples

```fortran
program intrinsics_fraction
    implicit none
    real :: x = 3.0
    print *, fraction(x)
end program
```

**Result**:

```
0.75
```

## See Also
