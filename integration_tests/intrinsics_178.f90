program intrinsics_178
    use iso_fortran_env, only: sp => real32, dp => real64
    real :: y
    real :: z
    real(dp) :: w
    real(dp) :: x
    real(sp), parameter :: x1 = rrspacing(178.1387e-4)
    real(dp), parameter :: y1 = rrspacing(1.01_dp)
    real(sp), parameter :: ar1(3) = rrspacing([1.1213, 2.1643, 3.613])
    real(dp), parameter :: ar2(3) = rrspacing([1.1213_dp, 2.1643_dp, 3.613_dp])
    real(sp) :: arr1(3)
    real(dp) :: arr2(3)
    arr1 = [11.90_sp, 12.00_sp, 45.09_sp]
    arr2 = [890.4_dp, 12.00_dp, 675.45_dp]
    x = 4.1
    y = 7.8
    z = 3.2
    w = 5.6

    print *, x1
    if (x1 - 9.56374900e+06_sp > 1e-5) error stop
    print*, y1
    if (y1 - 4.54863562364420100e+15_dp > 1e-5) error stop
    print*, ar1
    if (any(ar1 - [9.40614600e+06_sp, 9.07773200e+06_sp, 1.51540200e+07_sp] > 1e-5)) error stop
    print*, ar2
    if (any(ar2 - [5.04988626217053700e+15_dp, 4.87357033675898200e+15_dp, 8.13575272684480100e+15_dp] > 1e-5)) error stop
    
    print *, rrspacing(4.1)
    if ((rrspacing(4.1) - 8598323.00) > 1e-7) error stop

    print *, rrspacing(7.8)
    if ((rrspacing(7.8) - 16357786.0) > 1e-7) error stop

    print *, rrspacing(3.2_dp)
    if ((rrspacing(3.2_dp) - 7205759403792794.0_dp) > 1e-7) error stop

    print *, rrspacing(5.6_dp)
    if ((rrspacing(5.6_dp) - 6305039478318694.0_dp) > 1e-7) error stop

    print *, rrspacing(x)
    if ((rrspacing(x) - 461618961805475800.0_dp) > 1e-7) error stop

    print *, rrspacing(y)
    if ((rrspacing(y) - 16357786.0) > 1e-7) error stop

    print *, rrspacing(z)
    if ((rrspacing(z) - 13421773.0 ) > 1e-7) error stop

    print *, rrspacing(w)
    if ((rrspacing(w) - 630503947831869400.0) > 1e-7) error stop

    print *, rrspacing(4.1_dp)
    if ((rrspacing(4.1_dp) - 4616189618054758.0_dp) > 1e-7) error stop

    print *, rrspacing(7.8_dp)
    if ((rrspacing(7.8_dp) - 8782019273372467.0_dp) > 1e-7) error stop

    print *, rrspacing(3.2_dp)
    if ((rrspacing(3.2_dp) - 7205759403792794.0_dp) > 1e-7) error stop

    print *, rrspacing(5.6_dp)
    if ((rrspacing(5.6_dp) - 6305039478318694.0_dp) > 1e-7) error stop

    print *, kind(rrspacing(4.1))
    if (kind(rrspacing(4.1)) /= 4) error stop

    print *, kind(rrspacing(7.8_dp))
    if (kind(rrspacing(7.8_dp)) /= 8) error stop

    print *, kind(rrspacing(y))
    if (kind(rrspacing(y)) /= 4) error stop

    print *, kind(rrspacing(x))
    if (kind(rrspacing(x)) /= 8) error stop
    
    print *, rrspacing(arr1)
    if (any(rrspacing(arr1) - [1.24780540e+07_sp, 1.25829120e+07_sp, 1.18200730e+07_sp] > 1e-5)) error stop
    print *, rrspacing(arr2)
    if (any(rrspacing(arr2) - [7.83204122697400300e+15_sp, 6.75539944105574400e+15_sp, 5.94132103185039400e+15_sp] > 1e-5)) error stop

end program
