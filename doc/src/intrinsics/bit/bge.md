# bge(x, y): Bitwise Greater or Equal

Bitwise greater than or equal to.

## Declaration

### Syntax

```fortran
retval = bge(x, y);
elemental logical function bge(x, y)
```

### Arguments

`x` and `y` are integer input values. Both input values are of same kind.

### Return values

The return value is of type logical and of the default kind.

## Description

`bge(x, y)` calculates if two integer input values is bitwise greater than or
equal to another.

## Types

Supported input types is integer of 32 bit and 64 bit size.

```
interface bge
    module procedure bge32, bge64
end interface

contains

interface
    elemental logical function bge32(x, y)
	integer(int32), intent(in) :: x, y
	end function

    elemental logical function bge64(x, y)
	integer(int64), intent(in) :: x, y
	end function
end interface
```

## Examples

```fortran
program intrinsics_bge
    implicit none
    print *, bge(10, 4)
	print *, bge(10, -4)
	print *, bge(10, 10)
end program
```

**Result:**

```
true
false
true
```

## See Also

[bgt](bgt.md), [ble](ble.md), [blt](blt.md).
