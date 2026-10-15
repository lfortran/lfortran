program intrinsics_494
    ! aint and anint keep values that do not fit in integer(8), and infinity
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
    implicit none
    real(8) :: x, inf
    real(4) :: s
    x = 1.0d20
    s = 3.0e19
    inf = ieee_value(inf, ieee_positive_inf)
    print *, aint(x), anint(-x), aint(s), aint(inf)
    if (aint(x) /= x) error stop
    if (anint(-x) /= -x) error stop
    if (aint(s) /= s) error stop
    if (aint(s, kind=8) /= real(s, 8)) error stop
    if (aint(inf) /= inf) error stop
end program intrinsics_494
