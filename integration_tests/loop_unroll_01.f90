program loop_unroll_01
    ! Loops that the loop_unroll pass unrolls by its factor of 32 leave the
    ! DO variable with its Fortran value, and a loop with CYCLE is kept.
    implicit none
    integer :: i, s, a(64)

    ! 35 iterations: one group of 32 and 3 more.
    do i = 1, 35
        a(i) = i
    end do
    print *, i, a(35)
    if (i /= 36) error stop
    if (a(1) /= 1 .or. a(32) /= 32 .or. a(33) /= 33 .or. a(35) /= 35) error stop

    ! 64 iterations: two groups and none more.
    s = 0
    do i = 1, 64
        s = s + i
    end do
    print *, s, i
    if (s /= 2080) error stop
    if (i /= 65) error stop

    ! A negative step: 67 iterations, 200, 197, ..., 2.
    s = 0
    do i = 200, 1, -3
        s = s + i
    end do
    print *, s, i
    if (s /= 6767) error stop
    if (i /= -1) error stop

    ! CYCLE would skip the copies after it, so this loop is not unrolled.
    s = 0
    do i = 1, 40
        if (i - (i / 2) * 2 == 0) cycle
        s = s + i
    end do
    print *, s, i
    if (s /= 400) error stop
    if (i /= 41) error stop
end program
