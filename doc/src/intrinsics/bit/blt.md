# blt(x, y): Bitwise Less Than

Bitwise less than.

## Declaration

### Syntax

```fortran
retval = blt(x, y);
elemental logical function btt(x, y)
```

### Arguments

`x` and `y` are integer input values. Both input values are of same kind.

### Return values

The return value is of type logical and of the default kind.

## Description

`blt(x, y)` calculates if one integer input values is bitwise less than the
other.

## Types

Supported input types is integer of 32 bit and 64 bit size.

```
interface blt
    module procedure blt32, blt64
end interface

contains

interface
    elemental logical function blt32(x, y)
	integer(int32), intent(in) :: x, y
	end function

    elemental logical function blt64(x, y)
	integer(int64), intent(in) :: x, y
	end function
end interface
```

## Examples

```fortran
program intrinsics_blt
    implicit none
    print *, blt(10, 4)
	print *, blt(10, -4)
end program
```

**Result:**

```
false
true
```

## See Also

[bge](bge.md), [ble](ble.md), [bgt](bgt.md).
