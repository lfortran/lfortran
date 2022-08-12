# shiftl(x, shift): Shift Left

Logical shift left function.

## Declaration

### Syntax

```fortran
retval = shiftl(int(number), 31);
integer(int32) function shiftli32(i, shift)
```

### Arguments

`x` is an integer input value.

`shift` an unsigned integer value less than or equal to the bit size of `x`. The
possible values are 7, 31, and 63.

### Return values

The return value is of type integer and of the same kind as `x`.

## Description

**shiftl(x, shift)** logically left shifts `x` by `shift` number of bits. `shiftl`
shifts from LSB(**L**east **S**ignificant **B**it) to MSB(**M**ost **S**ignificant **B**it). Bits
shifted from the left end i.e., MSB bits are lost. Zeroes are appended to the
opposite right end.

## Types

Supported types in unsigned integer value `x` and unsigned integer value `shift`
from (7, 31, 63) less than or equal to bit size of `x`.

```
interface shiftl
    module procedure shiftli8, shiftli32, shiftli64
end interface

contains

interface
    integer(int8) function shiftli8(i, shift) result(r)
	integer(int8), intent(in) :: i
	integer :: shift
	end function

	integer(int32) function shiftli32(i, shift) result(r)
	integer(int32) :: i
	integer :: shift
	end function

	integer(int64) function shiftli64(i, shift) result(r)
	integer(int64) :: i
	integer :: shift
	end function
end interface
```

## Examples

```fortran
program intrinsics_shiftl
    implicit none
    integer, parameter :: x = kind(2)
    integer :: retval
    retval = shiftl(int(x), 7)
    print *, retval
end program
```

**Result:**

```
4
```

## See Also

[shiftr](shiftr.md).
