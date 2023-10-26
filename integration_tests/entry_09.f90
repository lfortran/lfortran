SUBROUTINE dinvr(x)
    IMPLICIT NONE
    DOUBLE PRECISION x
    DOUBLE PRECISION, SAVE :: big,small
    DOUBLE PRECISION zsmall, zbig
    print *, "small: ", small
    print *, "big: ", big
    if (abs(small - (-1.6d0)) > 1d-10) error stop
    if (abs(big - 10.9d0) > 1d-10) error stop

    entry distinv(zsmall, zbig)

    small = -1.6d0
    big = 10.9d0

    return

end subroutine

program entry_09
    implicit none
    double precision :: x
    x = 0.5d0

    call distinv(-1.6d0, 10.9d0)

    call dinvr(x)
end program

