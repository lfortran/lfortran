# bge(x, y): Bitwise Less or Equal

Bitwise less than or equal to.

## Declaration

### Syntax

```fortran
retval = ble(x, y);
elemental logical function ble(x, y)
```

### Arguments

`x` and `y` are integer input values. Both input values are of same kind.

### Return values

The return value is of type logical and of the default kind.

## Description

`ble(x, y)` calculates if one integer input values is bitwise less than or
equal to other.

## Types

Supported input types is integer of 32 bit and 64 bit size.

```
interface ble
    module procedure ble32, ble64
end interface

contains

interface
    elemental logical function ble32(x, y)
	integer(int32), intent(in) :: x, y
	end function

    elemental logical function ble64(x, y)
	integer(int64), intent(in) :: x, y
	end function
end interface
```

## Examples

```fortran
program intrinsics_ble
    implicit none
    print *, ble(10, 4)
	print *, ble(10, -4)
	print *, ble(10, 10)
end program
```

**Result:**

```
false
true
true
```

## See Also

[bgt](bgt.md), [bge](bge.md), [blt](blt.md).
