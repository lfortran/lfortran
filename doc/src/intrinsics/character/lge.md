# lge(x, y): Lexically Greater or Equal

Lexically greater than or equal.

## Declaration


### Syntax

```fortran
retval = lge(x, y)
function lge(x, y)
```

### Arguments

`x` input value of type character.

`y` input value of type character.

`x` and `y` can be seen as string A and string B.

### Return values

The return value is of logical `true` or `false` type.

`True` if `x` string is lexically greater than or equal to `y`.

`False` if they are not.

## Description

**lge(x, y)** determines if input string `x` is lexically greater than or equal
to input string `y`. The two strings in comparison are interpreted as
containing ASCII character codes.

## Types

Argument types should be of type character literal.

```fortran
module lfortran_intrinsic_string
    use, intrinsic :: iso_fortran_env, only: i64 => int64
implicit none

interface repeat
    module procedure repeati32, repeati64
end interface

contains

function lge(x, y) result(r)
    character(len=*),intent(in) :: x
    character(len=*),intent(in) :: y
    logical :: r
end function
```

## Examples

```fortran
program intrinsics_lge
    implicit none
    character(len = 10) :: s1 = 'abcde'
	character(len = 10) :: s2 = 'xyz'
    character(len = 10) :: s3 = 'AB'
	character(len = 10) :: s4 = 'AAB'
	print *, lge(s1, s2)
end program
```

**Result:**

```
false
true
```

## See Also

[len_trim](len_trim.md).
