program gpu_metal_609
    ! Single offloaded loops whose iterations run in several launches of the
    ! kernel (lfortran/lfortran#16569). The test is compiled with a small
    ! --gpu-chunk-limit, so each loop with more iterations than the limit
    ! runs in chunks, the last of them partial. Every element is checked.
    implicit none
    integer, parameter :: m = 100
    integer(8), parameter :: big = 5000000000_8
    integer :: a(m), b(-5:40), c(m), i, n, lo, hi
    integer(8) :: k
    integer :: d(21), g(2), h(3, 2)
    integer(8) :: l

    ! Constant extent.
    a = 0
    do concurrent (i = 1:m)
        a(i) = 3 * i
    end do
    do i = 1, m
        if (a(i) /= 3 * i) error stop 1
    end do

    ! Run-time bounds, a negative start.
    lo = -5
    hi = 40
    b = 0
    do concurrent (i = lo:hi)
        b(i) = i * i - 1
    end do
    do i = -5, 40
        if (b(i) /= i * i - 1) error stop 2
    end do

    ! A mask.
    n = m
    c = -1
    do concurrent (i = 1:n, mod(i, 3) == 0)
        c(i) = i
    end do
    do i = 1, m
        if (mod(i, 3) == 0) then
            if (c(i) /= i) error stop 3
        else
            if (c(i) /= -1) error stop 4
        end if
    end do

    ! A part of the array, run-time bounds.
    lo = 17
    hi = 83
    c = 0
    do concurrent (i = lo:hi)
        c(i) = a(i) + 1
    end do
    do i = 1, m
        if (i >= lo .and. i <= hi) then
            if (c(i) /= 3 * i + 1) error stop 5
        else
            if (c(i) /= 0) error stop 6
        end if
    end do

    ! No iterations.
    n = 0
    c = 7
    do concurrent (i = 1:n)
        c(i) = 0
    end do
    if (any(c /= 7)) error stop 7

    ! integer(8) indices beyond huge(0).
    d = 0
    do concurrent (k = 3000000000_8:3000000020_8)
        d(k - 2999999999_8) = int(k - 3000000000_8) + 1
    end do
    do i = 1, 21
        if (d(i) /= i) error stop 8
    end do

    ! integer(8) named-constant bounds beyond huge(0): within the limit 7,
    ! they stay constants in the kernel; with limit 1 they are split.
    g = 0
    do concurrent (l = big - 1:big)
        g(l - big + 2) = int(l - big) + 10
    end do
    if (g(1) /= 9 .or. g(2) /= 10) error stop 9
    h = 0
    do concurrent (k = 1:3, l = big - 1:big)
        h(k, l - big + 2) = 10 * int(k) + int(l - big + 2)
    end do
    do k = 1, 3
        do l = 1, 2
            if (h(k, l) /= 10 * k + l) error stop 10
        end do
    end do
    print *, "ok"
end program
