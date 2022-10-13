# size(x): Size of Array

Returns the size of an array `x`.

## Declaration

### Syntax

```fortran
retval = size(A[, dim[, kind]])
integer function size(x)
```

### Arguments

`A` the input array of any type or rank.

`dim` optional dimension, if present, `size` returns the size of this dimension.

`kind` optional the kind of the return value.

### Return values

`n` the size of an array (integer).

## Description

The `size` intrinsic function returns the size of an array. It returns the
product of all dimensions, unless the `dim` argument is specified, in which
case it only returns the size of this particular dimension. The `kind` argument
can be used to specify the integer kind of the result.

## Types

Supported argument type is integer.

## Examples

```fortran
program intrinsics_size
    implicit none
    real :: A(3, 4)
    print *, size(A)
    print *, size(A, 2)
end program
```

**Result**:

```
12
4
```

## See Also

[shape](), [reshape]().
