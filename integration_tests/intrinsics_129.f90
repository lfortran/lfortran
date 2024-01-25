program intrinsics_129
    complex(4) :: x, y(4)
    real(4) :: res(4)
    x = (1.0, 2.0)
    y = (3.0, 4.0)
    res = abs(y)
    print*, abs(y)
    print *, res
    if (any(abs(res - 5.0) > 1e-7)) error stop
    print *, abs(x)
    if (abs(abs(x) - 2.236068) > 1e-6) error stop
end program
