# bgt(x, y): Bitwise Greater Than

Bitwise greater than.

## Declaration

### Syntax

```fortran
retval = bgt(x, y);
elemental logical function bgt(x, y)
```

### Arguments

`x` and `y` are integer input values. Both input values are of same kind.

### Return values

The return value is of type logical and of the default kind.

## Description

`bgt(x, y)` calculates if one integer input values is bitwise greater than the
other.

## Types

Supported input types is integer of 32 bit and 64 bit size.

```
interface bgt
    module procedure bgt32, bgt64
end interface

contains

interface
    elemental logical function bgt32(x, y)
	integer(int32), intent(in) :: x, y
	end function

    elemental logical function bgt64(x, y)
	integer(int64), intent(in) :: x, y
	end function
end interface
```

## Examples

```fortran
program intrinsics_bgt
    implicit none
    print *, bgt(10, 4)
	print *, bgt(10, -4)
end program
```

**Result:**

```
true
false
```

## See Also

[bge](bge.md), [ble](ble.md), [blt](blt.md).
