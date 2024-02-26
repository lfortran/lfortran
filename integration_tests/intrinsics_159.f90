program intrinsics_159
    use iso_fortran_env, only: dp=>real64, sp=>real32
    complex(sp) :: x(2, 3)
    complex(sp) :: y(3, 2)
    complex(sp) :: res(2, 2)

    complex(dp) :: x_dp(2, 3)
    complex(dp) :: y_dp(3, 2)
    complex(dp) :: res_dp(2, 2)

    complex(sp) :: x_(5)
    complex(sp) :: y_(5, 1)
    complex(sp) :: res_(1)

    complex(sp) :: a(1, 5)
    complex(sp) :: b(5, 1)
    complex(sp) :: c(1, 1)

    complex(sp) :: e(4, 4)
    complex(sp) :: f(4, 4)

    real :: g(2, 3)
    real :: h(3, 2)

    integer :: l(2, 3)
    integer :: m(3, 2)
    integer :: i

    complex(sp) :: num

    num = (1.0, 2.0)
    x = reshape([num, num, num, num, num, num], [2, 3])
    y = transpose(x)
    res = matmul( x, y)
    print *, abs(sum(res))
    if (abs(abs(sum(res)) - 60.0) > 1e-8) error stop
    

    x_dp = reshape([ (i, i = 1, 6) ], [2, 3])
    y_dp = transpose(x_dp)
    res_dp = matmul( x_dp, y_dp )
    print *, abs(sum(res_dp))
    if (abs(abs(sum(res_dp)) - 179.0_dp) > 1e-12_dp) error stop


    x_ = [(i, i = 1, 5)]
    y_ = reshape([ (i, i = 1, 5) ], [5, 1])
    res_ = matmul( x_, y_)
    print *, abs(sum(res_))
    if (abs(abs(sum(res_)) - 55.0) > 1e-8) error stop

    a = reshape([ (i, i = 1, 5) ], [1, 5])
    b = reshape([ (i, i = 1, 5) ], [5, 1])
    c = matmul( a, b )
    print *, abs(sum(c))
    if (abs(abs(sum(c)) - 55.0) > 1e-8) error stop

    e = reshape([ (i, i = 1, 16) ], [4, 4])
    f = matmul( e, e )
    print *, abs(sum(f))
    if (abs(abs(sum(f)) - 4944.0) > 1e-8) error stop

    g = reshape([ (i, i = 1, 6) ], [2, 3])
    print *, abs(sum(matmul( g, y )))
    if (abs(abs(sum(matmul( g, y ))) - 93.9148560) > 1e-8) error stop

    h = reshape([ (i, i = 1, 6) ], [3, 2])
    print *, abs(sum(matmul( x, h )))
    if (abs(abs(sum(matmul( x, h ))) - 93.9148560) > 1e-8) error stop

    l = 901
    y = (12, -12)
    print *, abs(sum(matmul( l, y )))
    if (abs(abs(sum(matmul( l, y ))) - 183485.719) > 1e-8) error stop

    m = -158
    x = (10, -15)
    print *, abs(sum(matmul( x, m )))
    if (abs(abs(sum(matmul( x, m ))) - 34180.6250) > 1e-8) error stop

end program
