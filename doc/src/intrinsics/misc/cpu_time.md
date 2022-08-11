# cpu_time(x)

CPU elapsed time in seconds.

## Declaration

### Syntax

```fortran
retval = cpu_time(x)
pure subroutine cpu_time(x)
```

### Arguments

`x` the input value should be of type real with `intent(out)`.

### Return values

None.

## Description

**cpu_time(time)** returns a real value representing the elapsed CPU time in
seconds. If tine source is available, time will be reported with microsecond
resolution. If no time source is available, TIME is set to -1.0.

This is useful for testing segments of code to determine computation time.

For `cpu_time(time)` the absolute value is meaningless, only differences between
subsequent calls to this subroutine, as shown in the example below, should be used.

## Types

Supported input parameter types is real with `intent(out)`.

```fortran
pure subroutine cpu_time(t)
real(dp), intent(out) :: t
call c_cpu_time(t)
end subroutine
```

## Examples

```fortran
program intrinsics_cpu_time
    implicit none
	real(dp) :: t1, t2
	call cpu_time(t1)
	print *, "Some computation"
    call cpu_time(t2)
    print *, "Total time: ", t2-t1
end program
```

**Result:**

```
Some computation
20
```

## See Also
