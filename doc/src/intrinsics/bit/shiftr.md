# shiftr(x, shift): Shift Right

Logical shift right function.

## Declaration

### Syntax

```fortran
retval = shiftr(int(number), 31);
integer(int32) function shiftri32(i, shift)
```

### Arguments

`x` is an integer input value.

`shift` an unsigned integer value less than or equal to the bit size of `x`. The
possible values are 7, 31, and 63.

### Return values

The return value is of type integer and of the same kind as `x`.

## Description

**shiftr(x, shift)** logically right shifts `x` by `shift` number of bits.
`shiftr` shifts from MSB(**M**ost **S**ignificant **B**it) to LSB(**L**east **S**ignificant **B**it).
Bits shifted from the right end i.e., LSB bits are lost. Zeroes are appended to
the opposite left end.

## Types

Supported types in unsigned integer value `x` and unsigned integer value `shift`
from (7, 31, 63) less than or equal to bit size of `x`.

```
interface shiftr
    module procedure shiftri8, shiftri32, shiftri64
end interface

contains

interface
    integer(int8) function shiftri8(i, shift) result(r)
	integer(int8), intent(in) :: i
	integer :: shift
	end function

	integer(int32) function shiftri32(i, shift) result(r)
	integer(int32) :: i
	integer :: shift
	end function

	integer(int64) function shiftri64(i, shift) result(r)
	integer(int64) :: i
	integer :: shift
	end function
end interface
```

## Examples

```fortran
program intrinsics_shiftr
    implicit none
    integer, parameter :: x = kind(4)
    integer :: retval
    retval = shiftr(int(x), 7)
    print *, retval
end program
```

**Result:**

```
2
```

## See Also

[shiftl](shiftl.md).
