# bit_size(x): Bit Size

Bit size.

## Declaration

### Syntax

```fortran
retval = bit_size(x);
elemental logical function bit_size(x)
```

### Arguments

`x` is integer input value.

### Return values

The return value is of type integer.

## Description

`bit_size(x)` calculates the number of bits including sign bit of binary
representation of `x`.

## Types

Supported input types is integer.

```
module lfortran_intrinsic_builtin
implicit none

contains

interface
    integer function bit_size(x)
	integer, intent(in) :: x
	end function
end interface
```

## Examples

```fortran
program intrinsics_bit_size
    implicit none
    print *, bit_size(123)
	print *, bit_size(-1)
end program
```

**Result:**

```
32
32
```

## See Also
