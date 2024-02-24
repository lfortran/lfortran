program intrinsic_153
    integer, parameter :: dp = kind(0.d0)
    real :: i,j
    real(dp) :: x,y
    integer :: a, b
    integer(8) :: c, d
    real(dp) :: res_dp
    i = 30.0
    j = 20.0
    x = 10.0_dp
    y = 15.0_dp
    a = 30
    b = 20
    c = 10
    d = 15

    res_dp = dim(i, j)
    print *, res_dp
    if (abs(res_dp - (10.0_dp)) > 1e-14_dp) error stop

    res_dp = dim(x, y)
    print *, res_dp
    if (abs(res_dp - (0.0_dp)) > 1e-14_dp) error stop

    print *, dim(a, b)
    if (dim(a, b) /= 10) error stop

    print *, dim(c, d)
    if (dim(c, d) /= 0) error stop

    res_dp = dim(30.0, 20.0)
    print *, res_dp
    if (abs(res_dp - (10.0_dp)) > 1e-14_dp) error stop

    print *, dim(10.0, 15.0)
    if (abs(dim(10.0, 15.0) - (0.0)) > 1e-7) error stop

    print *, dim(30, 20)
    if (dim(30, 20) /= 10) error stop

    print *, dim(10, 15)
    if (dim(10, 15) /= 0) error stop

    print *, kind(dim(30, 20))
    if (kind(dim(30, 20)) /= 4) error stop

    print *, kind(dim(10_8, 15_8))
    if (kind(dim(10_8, 15_8)) /= 8) error stop

    print *, kind(dim(i, j))
    if (kind(dim(i, j)) /= 4) error stop

    print *, kind(dim(a, b))
    if (kind(dim(a, b)) /= 4) error stop

    print *, kind(dim(c, d))
    if (kind(dim(c, d)) /= 8) error stop

    print *, kind(dim(x, y))
    if (kind(dim(x, y)) /= 8) error stop

end program
