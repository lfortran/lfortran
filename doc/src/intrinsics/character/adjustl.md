# adjustl(string): Left Adjust String

Left adjust a string.

## Declaration

### Syntax

```fortran
retval = adjustl(string)
elemental function adjustl(string)
```

### Arguments

`string`: the input value must be of character type.

### Return value

The return value is of type character and of the same kind as of input value
`string`, with leading spaces removed and same number of spaces appended.

## Description

**adjustl(string)** adjusts input string by removing any leading spaces and
appending same number of spaces. 

## Types

Supported argument type is character.

```fortran

interface repeat
    module procedure repeati32, repeati64
end interface

contains

function adjustl(string)
    character(len=*), intent(in) :: string
end function
```

## Examples

```fortran
program intrinsics_adjustl
    implicit none
    character(*), parameter :: s1 = " A B "
    print *, adjustl(s1)
end program
```

**Result:**

```
A B  
```

## See Also

[lge](lge.md).
