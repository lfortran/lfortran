program do_concurrent_19
    ! The index-name of a DO CONCURRENT construct has construct scope, so a
    ! variable of the same name in the enclosing scope keeps its value.
    implicit none
    integer :: i, j, k, r(3), m(2, 3), s(4)
    integer :: i__dc
    integer(8) :: n8
    integer(8) :: r8(3)
    j = 100
    do concurrent (j = 1:3)
        r(j) = 2*j
    end do
    if (j /= 100) error stop
    if (any(r /= [2, 4, 6])) error stop

    i = -1
    j = -2
    do concurrent (i = 1:2, j = 1:3)
        m(i, j) = 10*i + j
    end do
    if (i /= -1) error stop
    if (j /= -2) error stop
    if (m(2, 3) /= 23 .or. m(1, 1) /= 11) error stop

    i = 7
    j = 8
    do concurrent (i = 1:2)
        do concurrent (j = 1:3)
            m(i, j) = i*j
        end do
    end do
    if (i /= 7 .or. j /= 8) error stop
    if (m(2, 3) /= 6) error stop

    k = 42
    do concurrent (k = 1:4, mod(k, 2) == 0)
        s(k) = k
    end do
    if (k /= 42) error stop
    if (s(2) /= 2 .or. s(4) /= 4) error stop

    j = 5
    do concurrent (j = 1:3)
        r(j) = get_host_j()
    end do
    if (j /= 5) error stop
    if (any(r /= 5)) error stop

    n8 = 9_8
    do concurrent (n8 = 1_8:3_8)
        r8(n8) = 4_8*n8
    end do
    if (n8 /= 9_8) error stop
    if (any(r8 /= [4_8, 8_8, 12_8])) error stop

    i__dc = 11
    call host_name_collision()

    call no_outer_index(s)
    if (any(s /= [1, 4, 9, 16])) error stop
    print *, "PASSED"

contains

    subroutine host_name_collision()
        ! The index variable created for `i` must not hide the host variable
        ! `i__dc`.
        integer :: q(3)
        do concurrent (i = 1:3)
            q(i) = i__dc + i
        end do
        if (any(q /= [12, 13, 14])) error stop
        if (i /= 7) error stop
    end subroutine

    pure integer function get_host_j()
        get_host_j = j
    end function

    subroutine no_outer_index(a)
        integer, intent(out) :: a(:)
        integer :: n
        n = size(a)
        do concurrent (n = 1:4)
            a(n) = n*n
        end do
        if (n /= 4) error stop
    end subroutine

end program
