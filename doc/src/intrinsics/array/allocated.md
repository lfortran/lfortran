# allocated(x): Status Check

Logical status of an allocatable integer.

## Declaration

### Syntax

```fortran
retval = allocated(x)
logical function allocated(x)
```

### Arguments

`x` is an integer input parameter.

### Return values

The return value is a logical scalar with the default logical kind type
parameter.

## Description

`allocated(x)` checks the allocation status of a integer input parameter. It
returns a logical value as `TRUE` if the input argument `x` is allocated,
`FALSE` otherwise.

## Types

Supported argument type is integer.

```fortran
module lfortran_intrinsic_builtin
implicit none

interface
    logical function allocated(x)
	integer, intent(in) :: x(:)
	end function
end interface

end module
```

## Examples

```fortran
program intrinsics_allocated
    implicit none
    integer :: i = 1
	real(1), allocatable :: x(:)
	if (.not. allocated(x))
		allocate(x(i))
    print *, allocated(i)
end program
```

**Result**:

```
TRUE
```

## See Also
