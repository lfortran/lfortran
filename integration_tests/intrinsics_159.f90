program intrinsics_159
    complex(4) :: x(2, 3)
    complex(4) :: y(3, 2)
    complex(4) :: res(2, 2)

    complex(8) :: x_8(2, 3)
    complex(8) :: y_8(3, 2)
    complex(8) :: res_8(2, 2)

    complex(4) :: x_(5)
    complex(4) :: y_(5, 1)
    complex(4) :: res_(1)

    complex(4) :: a(1, 5)
    complex(4) :: b(5, 1)
    complex(4) :: c(1, 1)

    complex(4) :: e(4, 4)
    complex(4) :: f(4, 4)

    real :: g(2, 3)
    real :: h(3, 2)

    integer :: l(2, 3)
    integer :: m(3, 2)
    integer :: i

    complex(4) :: num

    num = (1.0, 2.0)
    x = reshape([num, num, num, num, num, num], [2, 3])
    y = transpose(x)
    res = matmul( x, y)
    print *, abs(sum(res))
    if (abs(abs(sum(res)) - 60.0) > 1e-8) error stop
    

    x_8 = reshape([ (i, i = 1, 6) ], [2, 3])
    y_8 = transpose(x_8)
    res_8 = matmul( x_8, y_8 )
    print *, abs(sum(res_8))
    if (abs(abs(sum(res_8)) - 179.0D0) > 1e-12) error stop


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
