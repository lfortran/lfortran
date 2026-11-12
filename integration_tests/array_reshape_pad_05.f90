program array_reshape_pad_05
    ! `pad` that is an explicit-shape array with run-time bounds (dummy
    ! argument or automatic array) with a fixed-size `source`
    implicit none
    integer :: big(6)

    big = [7, 8, 9, 10, 11, 12]
    call check_rank1(big, 2, [7, 8, 7])
    call check_rank1(big, 5, [7, 8, 9])
    call check_rank1(big, 1, [7, 7, 7])
    call check_rank2(big, 2, [7, 8, 9])
    call check_rank2(big, 1, [7, 8, 7])
    call check_lower_bound(big, 2, [7, 8, 7])
    call check_automatic(3, [10, 20, 30])
    call check_automatic(2, [10, 20, 10])
    call check_real(real(big, 8), 2)
    call check_loop(big, 2, 1000000)

contains

    subroutine check_rank1(d, n, expected)
        integer, intent(in) :: n
        integer, intent(in) :: d(n), expected(3)
        integer :: w(3), r(2,3)
        w = [1, 2, 3]
        r = reshape(w, [2, 3], pad=d)
        print *, r
        if (any(r(:,1) /= [1, 2])) error stop
        if (r(1,2) /= 3) error stop
        if (any([r(2,2), r(:,3)] /= expected)) error stop
    end subroutine

    subroutine check_rank2(d, n, expected)
        integer, intent(in) :: n
        integer, intent(in) :: d(n,2), expected(3)
        integer :: w(3), r(2,3)
        w = [1, 2, 3]
        r = reshape(w, [2, 3], pad=d)
        if (any(r(:,1) /= [1, 2])) error stop
        if (r(1,2) /= 3) error stop
        if (any([r(2,2), r(:,3)] /= expected)) error stop
    end subroutine

    subroutine check_lower_bound(d, n, expected)
        integer, intent(in) :: n
        integer, intent(in) :: d(0:n-1), expected(3)
        integer :: w(3), r(2,3)
        w = [1, 2, 3]
        r = reshape(w, [2, 3], pad=d)
        if (any(r(:,1) /= [1, 2])) error stop
        if (r(1,2) /= 3) error stop
        if (any([r(2,2), r(:,3)] /= expected)) error stop
    end subroutine

    subroutine check_automatic(n, expected)
        integer, intent(in) :: n, expected(3)
        integer :: d(n), w(3), r(2,3), i
        do i = 1, n
            d(i) = 10*i
        end do
        w = [1, 2, 3]
        r = reshape(w, [2, 3], pad=d)
        if (any(r(:,1) /= [1, 2])) error stop
        if (r(1,2) /= 3) error stop
        if (any([r(2,2), r(:,3)] /= expected)) error stop
    end subroutine

    subroutine check_real(d, n)
        integer, intent(in) :: n
        real(8), intent(in) :: d(n)
        real(8) :: a(2,2), b(3,2)
        a = 1.5d0
        b = reshape(a, [3, 2], pad=d)
        if (any(abs(b(:,1) - 1.5d0) > 1d-12)) error stop
        if (any(abs(b(:,2) - [1.5d0, 7d0, 8d0]) > 1d-12)) error stop
    end subroutine

    ! In a loop, the reshape must not use more stack on every execution
    subroutine check_loop(d, n, m)
        integer, intent(in) :: n, m
        integer, intent(in) :: d(n)
        integer :: w(2,2), r(2,3), i
        integer(8) :: s
        w = reshape([1, 2, 3, 4], [2, 2])
        s = 0
        do i = 1, m
            r = reshape(w, [2, 3], pad=d)
            s = s + r(1,3) - r(2,3)
        end do
        if (s /= -int(m, 8)) error stop
    end subroutine

end program
