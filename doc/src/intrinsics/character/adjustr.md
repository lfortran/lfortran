# adjustr(string): Right Adjust String

Right adjust a string.

## Declaration

### Syntax

```fortran
retval = adjustr(string)
elemental function adjustr(string)
```

### Arguments

`string`: the input value must be of character type.

### Return value

The return value is of type character and of the same kind as of input value
`string`, with trailing spaces removed and same number of spaces added at the
start.

## Description

**adjustr(string)** adjusts input string by removing any trailing spaces and
adding same number of spaces at the start of the input string.

## Types

Supported argument type is character.

```fortran

interface repeat
    module procedure repeati32, repeati64
end interface

contains

function adjustr(string)
    character(len=*), intent(in) :: string
end function
```

## Examples

```fortran
program intrinsics_adjustr
    implicit none
    character(*), parameter :: s1 = "A B "
    print *, adjustr(s1)
end program
```

**Result:**

```
 A B
```

## See Also

[adjustl](adjustl.md).
