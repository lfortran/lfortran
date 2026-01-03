program intrinsics_291
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a
    real(sp) :: d
    integer :: i
    real(dp) :: log_runtime_dp(3) = [0.73925_dp, 4.6272183_dp, 82.1483927190_dp]
    real(sp) :: log_runtime_sp(3) = [0.73925, 4.6272183, 8.14839]
    real(dp) :: log_res_dp(3)
    real(sp) :: log_res_sp(3)
    real(dp) :: expected_dp(3) = [1.2399882923281214_dp, 13.907361652217805_dp, 1.1139704077610786E+121_dp]
    real(sp) :: expected_sp(3) = [1.23998833, 13.9073610, 6807.07861]

    real(dp), parameter :: res(3) = gamma([0.73925_dp, 4.6272183_dp, 82.1483927190_dp])
    real(sp), parameter :: res_sp(3) = gamma([0.73925, 4.6272183_sp, 8.14839])

    do i = 1, 3
        print *, res(i)
        if (abs(res(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, res_sp(i)
        if (abs(res_sp(i) - expected_sp(i)) > 1e-5) error stop
    end do

    log_res_dp = gamma(log_runtime_dp)
    log_res_sp = gamma(log_runtime_sp)

    do i = 1, 3
        print *, log_res_dp(i)
        if (abs(log_res_dp(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 3
        print *, log_res_sp(i)
        if (abs(log_res_sp(i) - expected_sp(i)) > 1e-5) error stop
    end do

    a = gamma(21.7282828_dp)
    d = gamma(11.63827)

    print *, a
    if (a - 2.22349180143608259e+19_dp > 1e-12_dp) error stop

    print *,gamma(21.7282828_dp)
    if (gamma(21.7282828_dp) - 2.22349180143608259e+19_dp > 1e-12_dp) error stop

    print *, d
    if (d - 16592656.0_sp > 1e-5) error stop

    print *, gamma(11.63827)
    if (gamma(11.63827) - 16592656.0_sp > 1e-5) error stop

    a = gamma(61.72812_dp)
    d = gamma(7.72818)

    print *, a
    if (a - 1.65731626595094650e+83_dp > 1e-12_dp) error stop

    print *,gamma(61.72812_dp)
    if (gamma(61.72812_dp) - 1.65731626595094650e+83_dp > 1e-12_dp) error stop

    print *, d
    if (d - 2.92850854e+03_sp > 1e-5) error stop

    print *, gamma(7.72818)
    if (gamma(7.72818) - 2.92850854e+03_sp > 1e-5) error stop

end program 