program do_concurrent_26
    ! DO CONCURRENT with one or several indices, any step, and CYCLE.
    implicit none
    integer :: i, j, a(4), b(15), s

    do concurrent (i = 1:4)
        a(i) = 2*i
    end do
    call check(a, 2, 4, 6, 8)

    do i = 1, 4
        a(i) = 0
    end do
    do concurrent (i = 4:1:-2)
        a(i) = i
    end do
    call check(a, 0, 2, 0, 4)

    do i = 1, 15
        b(i) = 0
    end do
    do concurrent (i = 1:3, j = 1:5:2)
        b(5*(i - 1) + j) = 10*i + j
    end do
    s = 0
    do i = 1, 15
        s = s + b(i)
    end do
    print *, s
    if (s /= 207) error stop

    do i = 1, 4
        a(i) = 0
    end do
    do concurrent (i = 1:4)
        if (i == 2 .or. i == 4) cycle
        a(i) = i
    end do
    call check(a, 1, 0, 3, 0)
contains
    subroutine check(x, x1, x2, x3, x4)
        integer, intent(in) :: x(4), x1, x2, x3, x4
        print *, x(1), x(2), x(3), x(4)
        if (x(1) /= x1 .or. x(2) /= x2 .or. x(3) /= x3 .or. x(4) /= x4) then
            error stop
        end if
    end subroutine
end program
