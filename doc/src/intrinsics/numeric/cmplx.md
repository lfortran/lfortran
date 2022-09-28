# cmplx(x [, [, kind]]): Convert to Complex

Conversion to complex type.

## Declaration

### Syntax

```fortran
retval = cmplx(x, y)
#define CMPLX(x, y)
```

`cmplx(x, y)` is one of the intrinsic present in ASR.

### Arguments

`x` the input value must be of integer, real, or complex.
`y` the optional input value must be of type integer or real. It should be only
present in case `x` is not complex.
`kind` the optional input value of integer expression type for initialisation
indicating the kind parameter of the result.

### Return values

The return value is of complex type, with a kind defined by input `kind` type
specified. If the `kind` is not specified, the return value is of default `kind`
`complex` type.

## Description

**cmplx(x, [, y [, kind]])** converts the input value to complex representation:

`x` to real component of complex number.
if `y` is present, it is converted to the imaginary component.

If `y` is not present, the imaginary component is set to 0.0.

If `x` is complex then `y` must not be present.

For `x` complex input value, the result is calculated using mathematical formula:

$x + iy$

$i$ the imaginary part is the result.

## Types

Supported argument types are integer, real, or complex.

```c
#define _Imaginary_I (I)
#define CMPLX(x, y) ((double complex)((double)(x) + _Imaginary_I * (double)(y)))
#define CMPLXF(x, y) ((float complex)((float)(x) + _Imaginary_I * (float)(y)))
#define CMPLXL(x, y) ((long double complex)((long double)(x) + \
                      _Imaginary_I * (long double)(y)))
```

## Examples

```fortran
program intrinsics_cmplx
    implicit none
	print *, cmplx(1.0, -3.0)
end program
```

**Result:**

```
1 - 3i
```

## See Also
