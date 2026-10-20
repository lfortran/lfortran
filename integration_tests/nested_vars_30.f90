! `z%re = hz()`, where the internal function hz writes z%im of the complex
! host variable z (#15310)
program nested_vars_30
    implicit none
    complex :: z
    z = (1.0, 1.0)
    z%re = hz()
    print *, z
    if (z /= (3.0, 4.0)) error stop
contains
    real function hz()
        z%im = 4.0
        hz = 3.0
    end function
end program
