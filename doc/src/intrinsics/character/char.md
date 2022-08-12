# char(x, [, kind]): Integer to Character

Integer to character conversion.

## Declaration

### Syntax

```fortran
retval = char(x)
character function char(x)
```

### Arguments

`x`: the input value of integer type.

`kind`: an optional input value of type integer constant expression for
initialization. This indicates the kind parameter of the result.

### Return value

The return value is of type character of length 1.

The return value is of kind `kind` if optional input value is passed, otherwise
the kind is default kind.

## Description

**char(x)** returns the character represented by the ASCII character set
at `x` position.

If `x` is outside the ASCII character set integer value, i.e., if `x` is not
represented in the ASCII character set, the return value is undefined.

It is a runtime builtin function.

## Types

Supported argument type is integer.

```fortran

interface

character function char(x)
    integer, intent(in) :: x
end function

end interface
```

## Examples

```fortran
program intrinsics_char
    implicit none
    character :: L = char(76)
    print *, L
end program
```

**Result:**

```
L
```

## See Also

[achar](achar.md), [iachar](iachar.md), [ichar](ichar.md)
