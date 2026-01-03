program intrinsics_171
    use iso_fortran_env, only: sp => real32, dp => real64
    real :: x
    real :: y
    real(dp) :: z
    real(dp) :: w
    real(sp), parameter :: x1 = fraction(178.1387e-4)
    real(dp), parameter :: y1 = fraction(1.01_dp)
    real(sp), parameter :: ar1(3) = fraction([1.1213, 2.1643, 3.613])
    real(dp), parameter :: ar2(3) = fraction([1.1213_dp, 2.1643_dp, 3.613_dp])
    real(sp) :: arr1(3)
    real(dp) :: arr2(3)
    arr1 = [11.90_sp, 12.00_sp, 45.09_sp]
    arr2 = [890.4_dp, 12.00_dp, 675.45_dp]
    x = 178.1387e-4
    y = 1.00
    z = -5.1
    w = 0.0

    print *, x1
    if (x1 - 0.57004_sp > 1e-5) error stop
    print*, y1
    if (y1 - 0.50500000000000000_dp > 1e-5) error stop
    print*, ar1
    if (any(ar1 - [0.560649991_sp, 0.541074991_sp, 0.903249979_sp] > 1e-5)) error stop
    print*, ar2
    if (any(ar2 - [0.56064999999999998_dp, 0.54107499999999997_dp, 0.90325000000000000_dp] > 1e-5)) error stop
    
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

    print *, fraction(arr1)
    if (any(fraction(arr1) - [0.743749976_sp, 0.750000000_sp, 0.704531252_sp] > 1e-5)) error stop
    print *, fraction(arr2)
    if (any(fraction(arr2) - [0.86953124999999998_dp, 0.75000000000000000_dp, 0.65961914062500004_dp] > 1e-5)) error stop

end program
