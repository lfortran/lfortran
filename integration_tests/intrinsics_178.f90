program intrinsics_178
    use iso_fortran_env, only: dp => real64
    real :: y
    real :: z
    real(dp) :: w
    real(dp) :: x 

    x = 4.1
    y = 7.8
    z = 3.2
    w = 5.6

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
    

end program
