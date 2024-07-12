program intrinsics_290
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a
    real(sp) :: d
    integer :: i
    real(dp) :: log_runtime_dp(3) = [0.875_dp, 3.367_dp, 2.140_dp]
    real(sp) :: log_runtime_sp(3) = [0.875, 3.367, 2.140]
    real(dp) :: log_res_dp(3)
    real(sp) :: log_res_sp(3)
    real(dp) :: expected_dp(3) = [-5.7991946977686754E-002_dp, 0.52724311638808863_dp, 0.33041377334919086_dp]
    real(sp) :: expected_sp(3) = [-5.79919480E-02_sp, 0.527243137_sp, 0.330413789]

    real(dp), parameter :: res(3) = log10([0.875_dp, 3.367_dp, 2.140_dp])
    real(sp), parameter :: res_sp(3) = log10([0.875, 3.367, 2.140])

    do i = 1, 3
        print *, res(i)
        if (abs(res(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res_sp(i)
        if (abs(res_sp(i) - expected_sp(i)) > 1e-5) error stop
    end do

    log_res_dp = log10(log_runtime_dp)
    log_res_sp = log10(log_runtime_sp)

    do i = 1, 3
        print *, log_res_dp(i)
        if (abs(log_res_dp(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, log_res_sp(i)
        if (abs(log_res_sp(i) - expected_sp(i)) > 1e-5) error stop
    end do

    a = log10(10.92839272_dp)
    d = log10(10.92839272)

    print *, a
    if (a - 1.03855629331626087e+00_dp > 1e-12_dp) error stop

    print *,log10(10.92839272_dp)
    if (log10(10.92839272_dp) - 1.03855629331626087e+00_dp > 1e-12_dp) error stop

    print *, d
    if (d - 1.03855634e+00_sp > 1e-5) error stop

    print *, log10(10.92839272)
    if (log10(10.92839272) - 1.03855634e+00_sp > 1e-5) error stop

    a = log10(83.728927_dp)
    d = log10(31.738222)

    print *, a
    if (a - 1.92287552568858744e+00_dp > 1e-12_dp) error stop

    print *,log10(83.728927_dp)
    if (log10(83.728927_dp) - 1.92287552568858744e+00_dp > 1e-12_dp) error stop

    print *, d
    if (d - 1.50158262e+00_sp > 1e-5) error stop

    print *, log10(31.738222)
    if (log10(31.738222) - 1.50158262e+00_sp > 1e-5) error stop

end program 
