program flip_sign
    implicit none
    integer :: number
    real :: x, eps = 1e-6

    number = 123
    x = 5.5

    if (modulo(number, 2) == 1 ) x = -x
    if (abs(x - (-5.5)) > eps) error stop

    number = 124
    x = 5.5
    if (modulo(number, 2) == 1 ) x = -x
    if (abs(x - (5.5)) > eps) error stop

end program
