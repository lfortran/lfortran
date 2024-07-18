program intrinsics_293
    use, intrinsic :: iso_fortran_env, only: dp => real64
    real(dp) :: a
    integer :: i
    real(dp) :: dgamma_runtime_dp(3) = [0.875_dp, 3.367_dp, 2.140_dp]
    real(dp) :: dgamma_res_dp(3)
    real(dp) :: expected_dp(3) = [1.0896523574228969_dp, 2.8783551043979587_dp, 1.0675143148681314_dp]

    real(dp), parameter :: res(3) = dgamma([0.875_dp, 3.367_dp, 2.140_dp])

    do i = 1, 3
        print *, res(i)
        if (abs(res(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    dgamma_res_dp = dgamma(dgamma_runtime_dp)

    do i = 1, 3
        print *, dgamma_res_dp(i)
        if (abs(dgamma_res_dp(i) - expected_dp(i)) > 1e-12_dp) error stop
    end do

    a = dgamma(10.92839272_dp)

    print *, a
    if (a - 3067135.1568285478_dp > 1e-12_dp) error stop

    print *,dgamma(10.92839272_dp)
    if (dgamma(10.92839272_dp) - 3067135.1568285478_dp > 1e-12_dp) error stop

    a = dgamma(83.728927_dp)

    print *, a
    ! if (abs(a - 1.1895370148607565E+124_dp) > 1e-12_dp) error stop

    print *,dgamma(83.728927_dp)
    ! if (abs(dgamma(83.728927_dp) - 1.1895370148607565E+124_dp) > 1e-12_dp) error stop

end program 