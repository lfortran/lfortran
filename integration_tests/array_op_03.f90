program main
    implicit none
    real :: x(2)
    real(8) :: a = -1.0
    x = -a
    if (abs(x(1) - (1)) > 1e-8) error stop
    if (abs(x(2) - (1)) > 1e-8) error stop
    print *, x
end program main
