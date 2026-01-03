program intrinsics_143
    real :: a(3), b(3)
    real :: res
    double precision :: c(5), d(5)
    double precision :: res_dp

    a = [1.0,2.0,3.0]
    b = [4.0,5.0,6.0]

    res = dot_product(a, b)
    print *, res
    if (abs(res - 32.0) > 1e-8) error stop
    res = dot_product([1.0,2.0,3.0], [4.0,5.0,6.0])
    print *, res
    if (abs(res - 32.0) > 1e-8) error stop

    c = [1.213D0, 2.113D0, 3.123D0, 4.12D0, 5.13D0]
    d = [4.213D0, 5.113D0, 6.123D0, 7.12D0, 8.13D0]

    res_dp = dot_product(c, d)
    print *, res_dp
    if (abs(res_dp - 106.077567D0) > 1e-12) error stop
    res_dp = dot_product([1.213D0, 2.113D0, 3.123D0, 4.12D0, 5.13D0], [4.213D0, 5.113D0, 6.123D0, 7.12D0, 8.13D0])
    print *, res_dp
    if (abs(res_dp - 106.077567D0) > 1e-12) error stop
end program
