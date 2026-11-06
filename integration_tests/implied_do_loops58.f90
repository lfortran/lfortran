program implied_do_loops58
    ! The size of an array constructor with a nested implied-do whose bounds
    ! depend on the variable of the outer one is the sum of the inner trip
    ! counts over the values of the outer variable, whatever its start.
    implicit none
    integer :: i, j, k, lo, hi
    integer(8) :: hi8
    integer, allocatable :: v(:)

    if (size([((j, j = i, 2), i = 0, 2)]) /= 6) error stop 1
    call check([((j, j = i, 2), i = 0, 2)], [0, 1, 2, 1, 2, 2])
    call check([((j, j = 1, i), i = 2, 3)], [1, 2, 1, 2, 3])
    call check([((j, j = 1, 2*i), i = 0, 2)], [1, 2, 1, 2, 3, 4])
    call check([((j, j = i, 4), i = 0, 4, 2)], [0, 1, 2, 3, 4, 2, 3, 4, 4])
    call check([((j, j = i, 2), -1, i = 0, 2)], [0, 1, 2, -1, 1, 2, -1, 2, -1])

    do lo = -2, 3
        do hi = lo - 1, lo + 3
            call triangle(lo, hi)
        end do
    end do

    lo = 0
    hi8 = 3
    if (size([((j, j = i, hi8), i = lo, hi8)]) /= 10) error stop 2
    allocate(v(10))
    v = [((j, j = i, hi8), i = lo, hi8)]
    k = 0
    do i = lo, int(hi8)
        do j = i, int(hi8)
            k = k + 1
            if (v(k) /= j) error stop 3
        end do
    end do
    print '(*(i0,1x))', [((j, j = i, 2), i = 0, 2)]

contains

    subroutine check(x, expected)
        integer, intent(in) :: x(:), expected(:)
        if (size(x) /= size(expected)) error stop 4
        if (any(x /= expected)) error stop 5
    end subroutine

    subroutine triangle(lo, hi)
        integer, intent(in) :: lo, hi
        integer :: i, j, n, m
        integer, allocatable :: v(:), w(:)
        n = (hi - lo + 1) * (hi - lo + 2) / 2
        if (size([((j, j = i, hi), i = lo, hi)]) /= n) error stop 6
        if (size([((j, j = lo, i), i = lo, hi)]) /= n) error stop 7
        if (size([((j, j = i, hi), i = lo, hi, 2)]) /= &
            (hi - lo + 2) / 2 * ((hi - lo + 2) - (hi - lo + 2) / 2)) error stop 8
        allocate(v(n), w(n))
        v = [((j, j = i, hi), i = lo, hi)]
        w = [((j, j = lo, i), i = lo, hi)]
        m = 0
        do i = lo, hi
            do j = i, hi
                m = m + 1
                if (v(m) /= j) error stop 9
            end do
        end do
        m = 0
        do i = lo, hi
            do j = lo, i
                m = m + 1
                if (w(m) /= j) error stop 10
            end do
        end do
    end subroutine

end program
