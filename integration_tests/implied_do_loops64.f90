program implied_do_loops64
    ! The size of an array constructor with a nested implied-do whose bounds
    ! depend on the variable of the outer one is the sum of the inner trip
    ! counts over the values of the outer variable, whatever its start.
    implicit none
    integer :: i, j, k, lo, hi
    integer(8) :: hi8
    integer :: w(3)
    integer, allocatable :: v(:)
    ! The same sizes in specification expressions, whose bounds are known at
    ! compile time here and only at run time in `automatic` and `nest`.
    integer :: d1(size([((j, j = 1, i), i = 1, 3)]))
    integer :: d2(size([((j, j = i, 3), i = 0, 3)]))
    integer :: d3(size([((j, j = 1, i * i), i = 1, 4)]))
    ! Too many iterations to count at compile time: the size is estimated,
    ! as for run-time bounds, and is at least the 80200 elements.
    integer :: d4(size([((j, j = i, 400), i = 1, 800)]))

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

    ! The inner trip counts are zero for some values of the outer variable,
    ! are not linear in it, or the nest is deeper: the size is the sum of the
    ! counts, each at least zero.
    lo = 5
    call check([((j, j = i, 3), i = 1, lo)], [1, 2, 3, 2, 3, 3])
    call check([((j, j = i, 3), i = 1, 5)], [1, 2, 3, 2, 3, 3])
    call check([((j, j = i, 3), i = 1, 7, 2)], [1, 2, 3, 3])
    call check([((j, j = 1, i, 2), i = 2, 4)], [1, 1, 3, 1, 3])
    call check([((j, j = 1, i, 2), i = 1, lo)], [1, 1, 1, 3, 1, 3, 1, 3, 5])
    call check([((j, j = i, 2), (j, j = i, 4), i = 1, lo)], &
        [1, 2, 1, 2, 3, 4, 2, 2, 3, 4, 3, 4, 4])
    call check([([(j, j = i, 3)], i = 1, lo)], [1, 2, 3, 2, 3, 3])
    hi = -1
    if (size([(((k, k = 1, hi), j = i, 3), i = 1, lo)]) /= 0) error stop 11
    if (size([((j, j = 1, i), i = lo, 1)]) /= 0) error stop 12
    hi = 1000
    if (size([((j, j = i, hi), i = 1, 2 * hi)]) /= 500500) error stop 13
    deallocate(v)
    allocate(v(500500))
    v = [((j, j = i, hi), i = 1, 2 * hi)]
    if (sum(int(v, 8)) /= 333833500_8) error stop 14
    if (size([((j, j = 1, i * i), i = 1, 4)]) /= 30) error stop 15
    if (sum([((j, j = 1, i * i), i = 1, 4)]) /= 192) error stop 16
    if (size([((j, j = 1, i * (6 - i)), i = 1, lo)]) /= 35) error stop 17
    call check([(((k, k = 1, j), j = 1, i), i = 1, 3)], [1, 1, 1, 2, 1, 1, 2, 1, 2, 3])
    if (size([(((k, k = 1, j), j = 1, i), i = 1, lo)]) /= 35) error stop 18
    if (sum([(((k, k = 1, j), j = 1, i), i = 1, lo)]) /= 70) error stop 19
    call check([(((k, k = j, 3), j = 1, 4), i = 1, 2)], &
        [1, 2, 3, 2, 3, 3, 1, 2, 3, 2, 3, 3])

    ! A stepped inner loop whose range is shorter than its step in the
    ! wrong direction, as `j = 4, 3, 2`, has no iterations.
    call check([((j, j = i, 3, 2), i = 1, lo)], [1, 3, 2, 3])
    call check([((j, j = i, 3, 2), i = 1, 5)], [1, 3, 2, 3])
    call check([((j, j = i, 1, -2), i = 0, 3)], [1, 2, 3, 1])
    if (size([((j, j = 1, i, 3), i = -4, 2)]) /= 2) error stop 26
    w = [10, 20, 30]
    call check([([w(i:3:2)], i = 1, lo)], [10, 30, 20, 30])
    deallocate(v)
    allocate(v(4))
    v = [((j, j = i, 3, 2), i = 1, lo)]
    if (any(v /= [1, 3, 2, 3])) error stop 27

    if (size(d1) /= 6) error stop 20
    if (size(d2) /= 10) error stop 21
    if (size(d3) /= 30) error stop 22
    if (size(d4) < 80200) error stop 28
    d4(:80200) = [((j, j = i, 400), i = 1, 800)]
    if (d4(80200) /= 400 .or. sum(int(d4(:80200), 8)) /= 21413400_8) error stop 29
    call automatic(3, 5)
    if (size(nest(3)) /= 6) error stop 25
    call check(nest(3), [1, 1, 2, 1, 2, 3])

contains

    subroutine automatic(n, m)
        integer, intent(in) :: n, m
        integer :: i, j
        integer :: a(size([((j, j = 1, i), i = 1, n)]))
        integer :: b(size([((j, j = i, n), i = 0, n)]))
        ! The inner trip counts are zero for i > n: the estimated size is
        ! at least the 6 elements.
        integer :: c(size([((j, j = i, n), i = 1, m)]))
        if (size(a) /= 6) error stop 23
        if (size(b) /= 10) error stop 24
        if (size(c) < 6) error stop 30
        c(:6) = [((j, j = i, n), i = 1, m)]
        if (any(c(:6) /= [1, 2, 3, 2, 3, 3])) error stop 31
    end subroutine

    function nest(n) result(res)
        integer, intent(in) :: n
        integer :: i, j
        integer :: res(size([((j, j = 1, i), i = 1, n)]))
        res = [((j, j = 1, i), i = 1, n)]
    end function

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
