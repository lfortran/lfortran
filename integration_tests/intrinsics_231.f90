program intrinsics_231
    use iso_fortran_env, only: dp => real64
    implicit none
    real(dp), parameter :: x1 = dgamma(1.0_dp)
    real(dp), parameter :: x2 = dgamma(3.14_dp)
    real(dp) :: x = 1.0_dp
    real(dp) :: y = 3.14_dp

    print *, x1
    if (abs(x1 - 1.0000000000000000_dp) > 1e-12) error stop
    print *, x2
    if (abs(x2 - 2.2844806338178012_dp) > 1e-12) error stop
    print *, dgamma(x)
    if (abs(dgamma(x) - 1.0000000000000000_dp) > 1e-12) error stop
    print *, dgamma(y)
    if (abs(dgamma(y) - 2.2844806338178012_dp) > 1e-12) error stop

end program