# achar(x, kind): To Character in ASCII set.

Character represented by in the ASCII character set.

## Declaration

### Syntax

```fortran
retval = achar(x, kind)
character(len=1) elemental function achar(x, kind)
```

### Arguments

`x`: the input value of integer type.
`kind`: an optional input value of type integer constant expression for
initialization.

### Return value

The return value is of type character of length 1.

The return value is of kind `kind` if optional input value is passed, otherwise
the kind is default kind.

## Description

**achar(x, kind)** returns the character represented by the ASCII character set
at `x` position.

If `x` is outside the ASCII character set integer value, i.e., if `x` is not
represented in the ASCII character set, the return value is undefined.

It is a runtime builtin function.

## Types

Supported argument type is integer.

```fortran

interface

character(len=1) function achar(x, kind)
    integer, intent(in) :: x
	integer, optional :: kind
end function

end interface
```

## Examples

```fortran
program intrinsics_achar
    implicit none
    character :: exclamation = achar(33)
    print *, exclamation
end program
```

**Result:**

```
!
```

## See Also

[lge](lge.md).
