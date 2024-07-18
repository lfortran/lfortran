program intrinsics_294
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a
    real(sp) :: d
    integer :: i
    real(dp) :: log_gamma_runtime_dp(3) = [0.875_dp, 3.367_dp, 2.140_dp]
    real(sp) :: log_gamma_runtime_sp(3) = [0.875, 3.367, 2.140]
    real(dp) :: log_gamma_res_dp(3)
    real(sp) :: log_gamma_res_sp(3)
    real(dp) :: expected_dp(3) = [8.5858707225334327E-002_dp, 1.0572189866765507_dp, 6.5332875743268432E-002_dp]
    real(sp) :: expected_sp(3) = [8.58587101E-02_sp, 1.05721915_sp, 6.53329268E-02]

    real(dp), parameter :: res(3) = log_gamma([0.875_dp, 3.367_dp, 2.140_dp])
    real(sp), parameter :: res_sp(3) = log_gamma([0.875, 3.367, 2.140])

    do i = 1, 3
        print *, res(i)
        if (abs(res(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res_sp(i)
        if (abs(res_sp(i) - expected_sp(i)) > 1e-5) error stop
    end do

    log_gamma_res_dp = log_gamma(log_gamma_runtime_dp)
    log_gamma_res_sp = log_gamma(log_gamma_runtime_sp)

    do i = 1, 3
        print *, log_gamma_res_dp(i)
        if (abs(log_gamma_res_dp(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, log_gamma_res_sp(i)
        if (abs(log_gamma_res_sp(i) - expected_sp(i)) > 1e-5) error stop
    end do

    a = log_gamma(10.92839272_dp)
    d = log_gamma(10.92839272)

    print *, a
    if (a - 14.936254510214283_dp > 1e-12_dp) error stop

    print *,log_gamma(10.92839272_dp)
    if (log_gamma(10.92839272_dp) - 14.936254510214283_dp > 1e-12_dp) error stop

    print *, d
    if (d - 14.9362535_sp > 1e-5) error stop

    print *, log_gamma(10.92839272)
    if (log_gamma(10.92839272) - 14.9362535_sp > 1e-5) error stop

    a = log_gamma(83.728927_dp)
    d = log_gamma(31.738222)

    print *, a
    if (a - 285.69411569953820_dp > 1e-12_dp) error stop

    print *,log_gamma(83.728927_dp)
    if (log_gamma(83.728927_dp) - 285.69411569953820_dp > 1e-12_dp) error stop

    print *, d
    if (d - 77.1901703_sp > 1e-5) error stop

    print *, log_gamma(31.738222)
    if (log_gamma(31.738222) - 77.1901703_sp > 1e-5) error stop

end program 