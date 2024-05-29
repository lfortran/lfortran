program intrinsics_01
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a, b, c(4)
    integer :: i
    real(dp) :: sqrt_run(3) = [5.678_dp, 892.272_dp, 72.312_dp]
    real(dp) :: log_run(3) = [0.875_dp, 3.367_dp, 2.140_dp]
    real(dp) :: abs_run(3) = [-723.182_dp, 9.124_dp, -123.145_dp]
    real(dp) :: sqrt_res(3), log_res(3), abs_res(3)
    real(dp) :: expected1(3) = [2.38285542994114508e+00_dp, 2.98709223158576087e+01_dp, 8.50364627674505336e+00_dp]
    real(dp) :: expected2(3) = [-0.13353139262452263_dp, 1.2140221401789375_dp, 0.76080582903376015_dp]
    real(dp) :: expected3(3) = [7.23182000000000000e+02_dp, 9.12400000000000000e+00_dp, 1.23145000000000000e+02_dp]

    real(dp), parameter :: res1(3) = sqrt([5.678_dp, 892.272_dp, 72.312_dp])
    real(dp), parameter :: res2(3) = log([0.875_dp, 3.367_dp, 2.140_dp])
    real(dp), parameter :: res3(3) = abs([-723.182_dp, 9.124_dp, -123.145_dp])

    do i = 1, 3
        print *, res1(i)
        if (abs(res1(i) - expected1(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res2(i)
        if (abs(res2(i) - expected2(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res3(i)
        if (abs(res3(i) - expected3(i)) > 1e-12_dp) error stop
    end do

    sqrt_res = sqrt(sqrt_run)
    log_res = log(log_run)
    abs_res = abs(abs_run)

    do i = 1, 3
        print *, sqrt_res(i)
        if (abs(sqrt_res(i) - expected1(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, log_res(i)
        if (abs(log_res(i) - expected2(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, abs_res(i)
        if (abs(abs_res(i) - expected3(i)) > 1e-12_dp) error stop
    end do

    a = 1.1_dp
    b = 1.2_dp
    if (b-a > 0.2_dp) error stop
    if (abs(b-a) > 0.2_dp) error stop
    if (abs(a-b) > 0.2_dp) error stop



    a = 4._dp
    if (abs(sqrt(a)-2._dp) > 1e-12_dp) error stop

    a = 4._dp
    if (abs(log(a)-1.3862943611198906_dp) > 1e-12_dp) error stop

    c(1) = -1._dp
    c(2) = -1._dp
    c(3) = -1._dp
    c(4) = -1._dp
    call random_number(c)
    do i = 1, 4
        if (c(i) < 0._dp) error stop
        if (c(i) > 1._dp) error stop
    end do
end
