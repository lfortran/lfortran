program intrinsics_171
    use iso_fortran_env, only: dp => real64
    real :: x
    real :: y
    real(dp) :: z
    real(dp) :: w
    x = 178.1387e-4
    y = 1.00
    z = -5.1
    w = 0.0

    print *, fraction(x)
    if (fraction(x) - 0.57004 > 1e-5) error stop

    print *, fraction(y)
    if (fraction(y) - 0.50000 > 1e-5) error stop

    print *, fraction(z)
    if (fraction(z) - (-0.63749998807907104_dp)  > 1e-8) error stop

    print *, fraction(w)
    if (fraction(w) - (0.0000000000000000_dp) > 1e-5) error stop

    print *, fraction(178.1387e-4)
    if (fraction(178.1387e-4) - (0.57004) > 1e-5) error stop

    print *, fraction(1.0)
    if (fraction(1.0) - (0.50000) > 1e-5) error stop

    print *, fraction(-5.1_dp)
    if (fraction(-5.1_dp) - (-0.637499988_dp ) > 1e-8) error stop
    
    print *, fraction(0.0_dp)
    if (fraction(0.0_dp) - (0.00000000_dp) > 1e-5) error stop
end program
