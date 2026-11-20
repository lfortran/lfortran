! nearest(x, -1.0) for x = 0 known only at run time: the smallest negative
! subnormal, also for x obtained by stepping down from it.
program nearest_below_zero_01
    implicit none
    real(8) :: x, d1, d2
    real(4) :: y, e1
    x = tiny(1.0d0)*epsilon(1.0d0)
    d1 = nearest(x, -1.0d0)
    d2 = nearest(d1, -1.0d0)
    print *, d1, d2
    if (d1 /= 0 .or. d2 /= -x) error stop "real(8)"
    y = 0
    e1 = nearest(y, -1.0)
    print *, e1
    if (e1 /= -tiny(1.0)*epsilon(1.0)) error stop "real(4)"
    if (nearest(e1, 1.0) /= 0) error stop "back to zero"
end program
