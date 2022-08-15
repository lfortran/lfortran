# kind(x): Kind of an Entity

Kind of an entity.

## Declaration

### Syntax

```fortran
retval = kind(x)
integer function kind(x)
```

### Arguments

`x` the input value, can be logical, integer, real, complex, or character. It
may be a scalar or array valued i.e., any intrinsic type.

### Return values

The return value is of integer type and of default integer kind.

## Description

**kind(x)** returns the kind parameter of the input argument `x`.

## Types

Supported argument types are logical, integer, real, complex, or character.

```fortran
module lfortran_intrinsic_kind
implicit none
contains

integer function kind(x) result(r)
logical(4), intent(in) :: x
r = 4
end function

integer function skind(x) result(r)
real(4), intent(in) :: x
r = 4
end function

integer function dkind(x) result(r)
real(8), intent(in) :: x
r = 8
end function

integer function lkind(x) result(r)
logical(4), intent(in) :: x
r = 4
end function

end module
```

## Examples

```fortran
program intrinsics_kind
    use lfortran_intrinsic_kind, only: kind
    implicit none
	logical :: l4d
	logical(4) :: l4
	print *, kind(l4d)
	print *, kind(l4)
end program
```

**Result:**

```
4
4
```

## See Also

None.
