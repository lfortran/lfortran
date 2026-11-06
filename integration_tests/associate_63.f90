program associate_63
    implicit none
    type :: t
        real :: x
    end type
    type(t) :: v
    real :: y1, y2, y3
    v%x = 1.0
    associate (z => v%x)
        y1 = exp(z)
        y2 = exp(z)
        y3 = 2.0 * exp(z)
    end associate
    if (abs(y1 - exp(1.0)) > 1e-5) error stop 1
    if (abs(y2 - exp(1.0)) > 1e-5) error stop 2
    if (abs(y3 - 2.0 * exp(1.0)) > 1e-5) error stop 3
    print *, "All tests passed"
end program
