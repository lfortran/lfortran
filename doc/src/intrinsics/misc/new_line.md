# new_line(x): New Line Character

New line character.

## Declaration

### Syntax

```fortran
retval = new_line(x)
function new_line(c)
```

### Arguments

The input parameter `x` must be of type character. It can be a scalar or an
array.

### Return values

The return value is a character of length one with the new line character
appended of the same kind as of `x`.

## Description

**new_line(x)** returns the new line character. The return value is the ASCII
newline character.

## Types

Supported input parameter types is character.

```fortran
function new_line(c) result(r)
character(len=1), intent(in) :: c
character(len=1) :: r
end function
```

## Examples

```fortran
program intrinsics_new_line
    implicit none
	print *, 'This is record 1.'//new_line('A')//'This is record 2.'
end program
```

**Result:**

```
This is record 1.A
This is record 2.
```

## See Also

None.
