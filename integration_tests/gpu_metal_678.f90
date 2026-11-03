program gpu_metal_678
    ! Nests with constant extents at and below the chunk limit
    ! (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=48: the 8 by 6 nest has exactly 48 iterations and
    ! the 4 by 5 one 20, so both run as one launch with their bounds kept
    ! as constants, a reduction included; the 8 by 7 nest, one value of `j`
    ! more, is split into chunks. Named constants as bounds are decided the
    ! same way: the n by m nest and its reduction run as one launch, and the
    ! n by m + 1 one is split along `j` only.
    implicit none
    integer, parameter :: n = 8, m = 6
    integer :: i, j, s, e, t
    integer :: a(8, 6), b(4, 5), c(8, 7), d(n, m), f(n, m + 1)

    a = 0
    do concurrent (i = 1:8, j = 1:6)
        a(i, j) = 10 * i + j
    end do
    b = 0
    do concurrent (i = 1:4, j = 1:5)
        b(i, j) = i * j
    end do
    c = 0
    do concurrent (i = 1:8, j = 1:7)
        c(i, j) = i - j
    end do
    s = 0
    do concurrent (i = 1:8, j = 1:6) reduce(+:s)
        s = s + a(i, j)
    end do
    d = 0
    do concurrent (i = 1:n, j = 1:m)
        d(i, j) = 10 * i + j
    end do
    t = 0
    do concurrent (i = 1:n, j = 1:m) reduce(+:t)
        t = t + d(i, j)
    end do
    f = 0
    do concurrent (i = 1:n, j = 1:m + 1)
        f(i, j) = i - j
    end do

    e = 0
    do j = 1, 7
        do i = 1, 8
            if (j <= 6) then
                if (a(i, j) /= 10 * i + j) error stop 1
                e = e + a(i, j)
                if (d(i, j) /= 10 * i + j) error stop 5
            end if
            if (c(i, j) /= i - j) error stop 2
            if (f(i, j) /= i - j) error stop 6
        end do
    end do
    do j = 1, 5
        do i = 1, 4
            if (b(i, j) /= i * j) error stop 3
        end do
    end do
    if (s /= e) error stop 4
    if (t /= e) error stop 7
    print *, "ok"
end program
