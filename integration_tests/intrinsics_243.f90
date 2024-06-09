program intrinsics_243
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a, b
    real(sp) :: d, e
    integer :: i
    real(dp) :: sqrt_runtime_dp(3) = [5.678_dp, 892.272_dp, 72.312_dp]
    real(dp) :: log_runtime_dp(3) = [0.875_dp, 3.367_dp, 2.140_dp]
    real(dp) :: abs_runtime_dp(3) = [-723.182_dp, 9.124_dp, -123.145_dp]
    real(sp) :: sqrt_runtime_sp(3) = [5.678, 892.272, 72.312]
    real(sp) :: log_runtime_sp(3) = [0.875, 3.367, 2.140]
    real(sp) :: abs_runtime_sp(3) = [-723.182, 9.124, -123.145]
    real(dp) :: sqrt_res_dp(3), log_res_dp(3), abs_res_dp(3)
    real(sp) :: sqrt_res_sp(3), log_res_sp(3), abs_res_sp(3)
    real(dp) :: expected1_dp(3) = [2.38285542994114508e+00_dp, 2.98709223158576087e+01_dp, 8.50364627674505336e+00_dp]
    real(dp) :: expected2_dp(3) = [-0.13353139262452263_dp, 1.2140221401789375_dp, 0.76080582903376015_dp]
    real(dp) :: expected3_dp(3) = [7.23182000000000000e+02_dp, 9.12400000000000000e+00_dp, 1.23145000000000000e+02_dp]
    real(sp) :: expected1_sp(3) = [2.38285542, 2.98709221e+01, 8.50364590]
    real(sp) :: expected2_sp(3) = [-1.33531392e-01, 1.21402216e+00, 7.60805905e-01]
    real(sp) :: expected3_sp(3) = [7.23182007e+02, 9.12399960e+00, 1.23144997e+02]

    real(dp), parameter :: res1(3) = sqrt([5.678_dp, 892.272_dp, 72.312_dp])
    real(dp), parameter :: res2(3) = log([0.875_dp, 3.367_dp, 2.140_dp])
    real(dp), parameter :: res3(3) = abs([-723.182_dp, 9.124_dp, -123.145_dp])
    real(sp), parameter :: res1_sp(3) = sqrt([5.678, 892.272, 72.312])
    real(sp), parameter :: res2_sp(3) = log([0.875, 3.367, 2.140])
    real(sp), parameter :: res3_sp(3) = abs([-723.182, 9.124, -123.145])

    do i = 1, 3
        print *, res1(i)
        if (abs(res1(i) - expected1_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res2(i)
        if (abs(res2(i) - expected2_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res3(i)
        if (abs(res3(i) - expected3_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res1_sp(i)
        if (abs(res1_sp(i) - expected1_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 3
        print *, res2_sp(i)
        if (abs(res2_sp(i) - expected2_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 3
        print *, res3_sp(i)
        if (abs(res3_sp(i) - expected3_sp(i)) > 1e-5) error stop
    end do

    sqrt_res_dp = sqrt(sqrt_runtime_dp)
    log_res_dp = log(log_runtime_dp)
    abs_res_dp = abs(abs_runtime_dp)
    sqrt_res_sp = sqrt(sqrt_runtime_sp)
    log_res_sp = log(log_runtime_sp)
    abs_res_sp = abs(abs_runtime_sp)

    do i = 1, 3
        print *, sqrt_res_dp(i)
        if (abs(sqrt_res_dp(i) - expected1_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, log_res_dp(i)
        if (abs(log_res_dp(i) - expected2_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, abs_res_dp(i)
        if (abs(abs_res_dp(i) - expected3_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, sqrt_res_sp(i)
        if (abs(sqrt_res_sp(i) - expected1_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 3
        print *, log_res_sp(i)
        if (abs(log_res_sp(i) - expected2_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 3
        print *, abs_res_sp(i)
        if (abs(abs_res_sp(i) - expected3_sp(i)) > 1e-5) error stop
    end do

    a = 1.1_dp
    b = 1.2_dp
    d = 1.1
    e = 1.2
    if (b-a > 0.2_dp) error stop
    if (abs(b-a) > 0.2_dp) error stop
    if (abs(a-b) > 0.2_dp) error stop
    if (abs(e-d) > 0.2) error stop
    if (abs(d-e) > 0.2) error stop
    if (abs(1.2_dp-1.1_dp) > 0.2_dp) error stop
    if (abs(1.1_dp-1.2_dp) > 0.2_dp) error stop
    if (abs(1.2-1.1) > 0.2) error stop
    if (abs(1.1-1.2) > 0.2) error stop


    a = 4._dp
    d = 4.
    if (abs(sqrt(a)-2._dp) > 1e-12_dp) error stop
    if (abs(sqrt(4._dp)-2._dp) > 1e-12_dp) error stop
    if (abs(sqrt(d)-2.) > 1e-5) error stop
    if (abs(sqrt(4.)-2.) > 1e-5) error stop

    a = 4._dp
    d = 4.
    if (abs(log(a)-1.3862943611198906_dp) > 1e-12_dp) error stop
    if (abs(log(4._dp)-1.3862943611198906_dp) > 1e-12_dp) error stop
    if (abs(log(d)-1.3862943611198906) > 1e-5) error stop
    if (abs(log(4.)-1.3862943611198906) > 1e-5) error stop

end