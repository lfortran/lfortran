# btest(x, pos): Bit Test at Position

Bit test.

## Declaration

### Syntax

```fortran
retval = btest(x, pos);
elemental logical function btest(x, pos)
```

### Arguments

`x` and `pos` are integer input values. Both input values are of same kind.
`pos` represents position in `x`.

### Return values

The return value is of type logical.

## Description

`btest(x, pos)` calculates if `pos` bit in input integer value `x` is set. The
counting of the bits starts at 0, at least significant bit (LSB) i.e., the
rightmost bit in `x`.

If `pos` less than 0 or greater than `bit_size()`, `btest(x, pos)` errors with
not allowed message.

## Types

Supported input types is integer of 32 bit and 64 bit size.

```
interface btest
    module procedure btest32, btest64
end interface

contains

interface
    elemental logical function btest32(x, pos)
	integer(int32), intent(in) :: x
	integer, intent(in) :: pos
	end function

    elemental logical function btest64(x, pos)
	integer(int64), intent(in) :: x
	integer, intent(in) :: pos
	end function
end interface
```

## Examples

```fortran
program intrinsics_btest
    implicit none
    print *, btest(2, 0)
end program
```

**Result:**

```
false
```

## See Also
