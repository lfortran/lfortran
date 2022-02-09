program flip_sign
    implicit none
    real :: x, eps = 1e-6

    x = 5.5

    x = x * sign(1._4, eps)
    if (abs(x - 5.5) > eps) error stop

    x = x * sign(1._4, -eps)
    if (abs(x + 5.5) > eps) error stop

end program
