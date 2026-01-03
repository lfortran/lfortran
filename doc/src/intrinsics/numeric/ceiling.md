# ceiling(x): Integer Ceiling

Integer ceiling function

## Declaration

### Syntax

```fortran
retval = ceiling(x)
elemental integer function ceiling(x)
```

### Arguments

The input value `x` may be of type real or integer.

### Return values

The return value is of type integer and nearest greater integer.

## Description

**ceiling(x)** returns the least integer greater than or equal to `x`.

## Types

Supported input parameter types are integer and real.

```fortran
interface ceiling
    module procedure sceiling, dceiling
end interface

contains

elemental integer function sceiling(x)
real(sp), intent(in) :: x
end function

elemental integer function dceiling(x)
real(dp), intent(in) :: x
end function
```

## Examples

```fortran
program intrinsics_ceiling
   implicit none
   real :: x = 63.29
   print *, ceiling(x)
   print *, ceiling(-63.29)
end program
```

**Result:**

```
64
-63
```

## See Also

[floor](floor.md), [mod](mod.md).
