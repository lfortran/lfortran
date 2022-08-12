# dim(x, y): Positive Difference or 0

Positive difference or 0.

## Declaration

### Syntax

```fortran
retval = dim(x, y)
```

### Arguments

`x` the input value must be of type real or integer.
`y` the input value must of of the same type and kind as of `x`. 

### Return values

The return value is of type integer or real.

## Description

**dim(x)** computes the difference $x - y$, if the result is positive, otherwise
returns 0.

## Types

Supported argument types are real and integer.

## Examples

```fortran
program intrinsics_dim
	print *, dim(2, 3)
	print *, dim(3, 2)
end program
```

**Result:**

```
0
1
```

## See Also
