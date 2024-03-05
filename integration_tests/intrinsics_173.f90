program intrinsics_171
    real :: x
    real :: y
    real(8) :: z
    real(8) :: w
    x = 178.1387e-4
    y = 1.00
    z = -5.1
    w = 0.0

    print *, fraction(x)
    if (fraction(x) - 0.57004 > 1e-5) error stop

    print *, fraction(y)
    if (fraction(y) - 0.50000 > 1e-5) error stop

    print *, fraction(z)
    if (fraction(z) - (-0.63749998807907104_8)  > 1e-8) error stop

    print *, fraction(w)
    if (fraction(w) - (0.0000000000000000_8) > 1e-5) error stop

    print *, fraction(178.1387e-4)
    if (fraction(178.1387e-4) - (0.57004) > 1e-5) error stop

    print *, fraction(1.0)
    if (fraction(1.0) - (0.50000) > 1e-5) error stop

    print *, fraction(-5.1_8)
    if (fraction(-5.1_8) - (-0.637499988 ) > 1e-8) error stop
    
    print *, fraction(0.0_8)
    if (fraction(0.0_8) - (0.00000000) > 1e-5) error stop
end program
