program expr_23
    ! A sign that starts an expression applies to the whole product
    ! or quotient that follows it: -a*b/c is -(a*b/c), not (-a)*b/c
    implicit none
    real :: a, b, c, x
    integer :: i, j
    a = 3.0
    b = 4.0
    c = 2.0
    x = -a*b/c
    if (x /= -6.0) error stop
    x = -a**2*b
    if (x /= -36.0) error stop
    x = +a*b
    if (x /= 12.0) error stop
    x = (-a)*b
    if (x /= -12.0) error stop
    x = c - a*b
    if (x /= -10.0) error stop
    if (.not. (c > -a*b)) error stop
    i = 7
    j = 2
    if (-i/j*j /= -6) error stop
end program expr_23
