program main
    implicit none
    real :: x(2)
    x = -1.0
    if (abs(x(1) - (-1)) > 1e-8) error stop
    if (abs(x(2) - (-1)) > 1e-8) error stop
    print *, x
end program main
    