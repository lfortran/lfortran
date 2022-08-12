# command_argument_count()

Get number of command line arguments.

## Declaration

### Syntax

```fortran
retval = command_argument_count()
function command_argument_count()
```

### Arguments

None.

### Return values

The return value is of integer type and of default kind.

## Description

**command_argument_count()** returns the number of arguments passed on the
command line when the named program was invoked.

## Types

Any.

## Examples

```fortran
program intrinsics_command_argument_count
    implicit none
	print *, command_argument_count()
end program
```

**Result:**

```
intrinsics_command_argument_count a b
2
```

## See Also
