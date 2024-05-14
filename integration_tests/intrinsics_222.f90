program intrinsics_221
    use, intrinsic :: iso_fortran_env, only: dp => real64
    complex(8) :: a, b, c, d
    real(dp) :: expected(4)
    integer :: i

    real(dp), parameter :: result(4) = dreal([(1.0_8, 2.0_8), (-3.7280_8, 4.0_8), (-1.0_8, -1.0_8), (10.829_8, -189.0_8)])
    expected = [1.0000000000000000_dp, -3.7280000000000000_dp, -1.0000000000000000_dp, 10.8290000000000000_dp]

    do i = 1, size(expected)
        print *, result(i)
        if (abs(result(i) - expected(i)) > 1e-12_dp) error stop
    end do

    a = (1.0_8, 2.0_8)
    b = (-3.7280_8, 4.0_8)
    c = (-1.0_8, -1.0_8)
    d = (10.829_8, -189.0_8)

    print *, dreal(a)
    if (abs(dreal(a) - 1.0000000000000000_dp) > 1e-12_dp) error stop

    print *, dreal(b)
    if (abs(dreal(b) + 3.7280000000000000_dp) > 1e-12_dp) error stop

    print *, dreal(c)
    if (abs(dreal(c) + 1.0000000000000000_dp) > 1e-12_dp) error stop

    print *, dreal(d)
    if (abs(dreal(d) - 10.8290000000000000_dp) > 1e-12_dp) error stop

end program