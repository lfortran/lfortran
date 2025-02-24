program intrinsics_366
    implicit none
    integer :: x, v, ntz, y, i
    integer, parameter :: ITRLim = 80

    ! Testing Collatz sequence using bit intrinsics

    ! Example1: using shiftl and ibclr
    x = 37
    do i = 1, ITRLim
        if (btest(x, 0)) then  ! odd
            v = shiftl(x, 2) - ibclr(x, 0)  ! v = 4*x - (x-1)
        else
            v = x
        end if
        ntz = trailz(v)
        y = shiftr(v, ntz)
        print '(3i8)', x, v, y
        x = y
        if (x == 1) exit
    end do

    if (v /= 16) error stop
    if (y /= 1) error stop

    ! Example2: using simple 3x + 1
    x = 37
    do i = 1, ITRLim
        if (btest(x, 0)) then  ! odd
            v = 3 * x + 1
        else
            v = x
        end if
        ntz = trailz(v)
        y = shiftr(v, ntz)
        print '(3i8, 4x, i2)', x, v, y, ntz
        x = y
        if (x == 1) exit
    end do

    if (v /= 16) error stop
    if (y /= 1) error stop
    if (ntz /= 4) error stop

end program