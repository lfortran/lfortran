# len_trim(string): Length Ignoring Trailing Blanks

Length of a character string ignoring trailing blank character(s).

## Declaration

### Syntax

```fortran
retval = len_trim(string)
integer elemental function len_trim(string)
```

### Arguments

`string` the input value of character type.

### Return value

The return value is of type unsigned integer.

## Description

**len_trim(string)** returns the length of the character argument without
including trailing blank character(s).

## Types

Supported argument type is character scalar.

```fortran

interface repeat
    module procedure repeati32, repeati64
end interface

contains

integer elemental function len_trim(string)
    character(len=*), intent(in) :: string
end function
```

## Examples

```fortran
program intrinsics_len_trim
    implicit none
    character(*), parameter :: s1 = " A B "
    integer:: retval
    retval = len_trim(s1)
    print *, retval
end program
```

**Result:**

```
4
```

## See Also

[lge](lge.md).
