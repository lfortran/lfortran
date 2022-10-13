program div_to_mul
    implicit none
    real :: x = 3.14, eps = 1e-6
    real, parameter :: pi = 3.14
    integer :: y = 2

    if (abs(x/pi - 1.0) > eps) error stop
    if (abs(x/2.0 - 1.57) > eps) error stop
    if (abs(x/2.0_8 - 1.57) > eps) error stop
    if (abs(x/2 - 1.57) > eps) error stop
    if (abs(y/2 - 1) > eps) error stop

end program
