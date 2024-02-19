program intrinsics_145
    complex :: a(3), b(3)
    complex :: res
    complex(8) :: c(3), d(3)
    complex(8) :: res_8

    a = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
    b = [(7.0, 8.0), (9.0, 10.0), (11.0, 12.0)]

    res = dot_product([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)], [(7.0, 8.0), (9.0, 10.0), (11.0, 12.0)])
    print *, abs(res)
    if (abs(abs(res) - 217.745270) > 1e-8) error stop

    res = dot_product(a, b)
    print *, abs(res)
    if (abs(abs(res) - 217.745270) > 1e-8) error stop

    c = [(1.0D0, 2.0D0), (3.0D0, 4.0D0), (5.0D0, 6.0D0)]
    d = [(7.0D0, 8.0D0), (9.0D0, 10.0D0), (11.0D0, 12.0D0)]

    res_8 = dot_product(c, d)
    print *, abs(res_8)
    if (abs(abs(res_8) - 217.74526401279087D0) > 1e-12) error stop

    res_8 = dot_product([(1.0D0, 2.0D0), (3.0D0, 4.0D0), (5.0D0, 6.0D0)], [(7.0D0, 8.0D0), (9.0D0, 10.0D0), (11.0D0, 12.0D0)])
    print *, abs(res_8)
    if (abs(abs(res_8) - 217.74526401279087D0) > 1e-12) error stop

end program
