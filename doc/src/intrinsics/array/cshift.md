# cshift(A, shift [, dim]): Circular Shift

Circular shift elements of an array.

## Declaration

### Syntax

```fortran
retval = cshift(A, shift [, dim])
integer function cshift(A, shift, [, dim])
```

### Arguments

`A` the input array of any type or rank.

`shift` the input value of integer type.

`dim` optional dimension, if present, `cshift` returns the result of this dimension.

### Return values

The return value is of input array type and rank as the `A` array input argument.

## Description

**cshift(A, shift [, dim])** performs a circular shift on elements of `A` array
along the dimension of `dim`. Default value of `dim` is 1, used when `dim` is
not passed.

If the rank of array is 1, then all elements of array are shifted by `shift`
places. If rank is greater than one, then all complexte rank one sections of
array along the given dimension are shifted.

Elements shifted out one end of each rank one section are shifted back in the
other end.

## Types

Supported argument type is array for `A` and integer for `shift` and `dim`.

## Examples

```fortran
program intrinsics_cshift
    implicit none
	integer, dimension(5):: a = [1, 2, 3, 4, 5]
	print '(5i3)', a
	a = cshift(a, shift = 2)
    print *, size(A, 2)
end program
```

**Result**:

```
1, 2, 3
4, 5, 1
```

## See Also
