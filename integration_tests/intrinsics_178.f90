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

    print *, Rrspacing(4.1)
    if ((Rrspacing(4.1) - 8598323.00) > 1e-7) error stop

    print *, Rrspacing(7.8)
    if ((Rrspacing(7.8) - 16357786.0) > 1e-7) error stop

    print *, Rrspacing(3.2_dp)
    if ((Rrspacing(3.2_dp) - 7205759403792794.0_dp) > 1e-7) error stop

    print *, Rrspacing(5.6_dp)
    if ((Rrspacing(5.6_dp) - 6305039478318694.0_dp) > 1e-7) error stop

    print *, Rrspacing(x)
    if ((Rrspacing(x) - 461618961805475800.0_dp) > 1e-7) error stop

    print *, Rrspacing(y)
    if ((Rrspacing(y) - 16357786.0) > 1e-7) error stop

    print *, Rrspacing(z)
    if ((Rrspacing(z) - 13421773.0 ) > 1e-7) error stop

    print *, Rrspacing(w)
    if ((Rrspacing(w) - 630503947831869400.0) > 1e-7) error stop

    print *, Rrspacing(4.1_dp)
    if ((Rrspacing(4.1_dp) - 4616189618054758.0_dp) > 1e-7) error stop

    print *, Rrspacing(7.8_dp)
    if ((Rrspacing(7.8_dp) - 8782019273372467.0_dp) > 1e-7) error stop

    print *, Rrspacing(3.2_dp)
    if ((Rrspacing(3.2_dp) - 7205759403792794.0_dp) > 1e-7) error stop

    print *, Rrspacing(5.6_dp)
    if ((Rrspacing(5.6_dp) - 6305039478318694.0_dp) > 1e-7) error stop

    print *, kind(Rrspacing(4.1))
    if (kind(Rrspacing(4.1)) /= 4) error stop

    print *, kind(Rrspacing(7.8_dp))
    if (kind(Rrspacing(7.8_dp)) /= 8) error stop

    print *, kind(Rrspacing(y))
    if (kind(Rrspacing(y)) /= 4) error stop

    print *, kind(Rrspacing(x))
    if (kind(Rrspacing(x)) /= 8) error stop
    

end program
