program do_loop_21
    ! The end and the step of a DO loop are evaluated once, before the
    ! loop, and the DO variable ends one step past its last value.
    implicit none
    integer :: i, n, s, cnt, k
    integer(8) :: i8

    ! The loop modifies its end and its step.
    n = 5
    cnt = 0
    do i = 1, n
        n = n - 1
        cnt = cnt + 1
    end do
    print *, i, n, cnt
    if (i /= 6 .or. n /= 0 .or. cnt /= 5) error stop

    s = 2
    cnt = 0
    do i = 1, 10, s
        s = s + 1
        cnt = cnt + 1
    end do
    print *, i, s, cnt
    if (i /= 11 .or. s /= 7 .or. cnt /= 5) error stop

    ! A call modifies the end through its argument.
    n = 4
    cnt = 0
    do i = 1, n
        call decrement(n)
        cnt = cnt + 1
    end do
    print *, i, n, cnt
    if (i /= 5 .or. n /= 0 .or. cnt /= 4) error stop

    ! Steps whose sign is known only at run time.
    call run_step(10, 1, -3, cnt, k)
    if (cnt /= 4 .or. k /= -2) error stop
    call run_step(10, 1, 3, cnt, k)
    if (cnt /= 0 .or. k /= 10) error stop
    call run_step(1, 10, 4, cnt, k)
    if (cnt /= 3 .or. k /= 13) error stop

    ! Constant steps, either sign.
    cnt = 0
    do i = 10, 1, -4
        cnt = cnt + i
    end do
    print *, i, cnt
    if (i /= -2 .or. cnt /= 18) error stop
    cnt = 0
    do i = 10, 1, -1
        cnt = cnt + 1
    end do
    if (i /= 0 .or. cnt /= 10) error stop
    cnt = 0
    do i = 1, 10, 3
        cnt = cnt + i
    end do
    if (i /= 13 .or. cnt /= 22) error stop

    ! A loop that runs no iteration leaves the start.
    k = 7
    do i = 5, k - 10
        error stop
    end do
    if (i /= 5) error stop

    ! A DO variable of another kind.
    cnt = 0
    do i8 = 3_8, 9_8, 2_8
        cnt = cnt + 1
    end do
    print *, i8, cnt
    if (i8 /= 11_8 .or. cnt /= 4) error stop

    ! A loop without a loop control.
    i = 0
    do
        i = i + 1
        if (i == 6) exit
    end do
    if (i /= 6) error stop
contains
    subroutine decrement(m)
        integer, intent(inout) :: m
        m = m - 1
    end subroutine

    subroutine run_step(a, b, st, cnt, last)
        integer, intent(in) :: a, b, st
        integer, intent(out) :: cnt, last
        integer :: m
        cnt = 0
        do m = a, b, st
            cnt = cnt + 1
        end do
        last = m
    end subroutine
end program
