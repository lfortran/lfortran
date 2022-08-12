# digits(x)

Significant binary digits.

## Declaration

### Syntax

```fortran
retval = digits(x)
```

### Arguments

`x` the input value must be of type real or integer.

### Return values

The return value is of type integer.

## Description

**digits(x)** returns the number of significant binary digits of the internal
mopdel representation of `x`.

## Types

Supported argument types are real and integer.

## Examples

```fortran
program intrinsics_digits
	print *, digits(3.1)
end program
```

**Result:**

```
24
```

## See Also
