program arrays_150
    implicit none
    integer :: a(3), i, n, repeat
    real(8) :: b(-1:1)

    n = 3
    i = 19
    do repeat = 1, 3
        a = [(i + repeat, i=1,n)]
        a = [(a(n + 1 - i), i=1,n)]
        if (a(1) /= repeat + 3) error stop
        if (a(2) /= repeat + 2) error stop
        if (a(3) /= repeat + 1) error stop
        b = [(i * 1.0_8, i=1,n)]
        if (b(-1) /= 1.0_8 .or. b(1) /= 3.0_8) error stop
        if (i /= 19) error stop
    end do
    do repeat = 1, 2
        do n = 0, 4
            call check([(i, i=1,n)], n)
        end do
    end do
    if (i /= 19) error stop
    call check_local(2)
    call check_local(4)
contains
    subroutine check_local(n)
        integer, intent(in) :: n
        integer :: values(n), j
        values = [(j, j=1,n)]
        if (values(n) /= n) error stop
    end subroutine

    subroutine check(values, n)
        integer, intent(in) :: values(:), n
        integer :: j
        if (size(values) /= n) error stop
        do j = 1, n
            if (values(j) /= j) error stop
        end do
    end subroutine
end program
