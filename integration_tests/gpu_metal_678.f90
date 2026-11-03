program gpu_metal_678
    ! Nests with constant extents at and below the chunk limit
    ! (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=48: the 8 by 6 nest has exactly 48 iterations and
    ! the 4 by 5 one 20, so both run as one launch with their bounds kept
    ! as constants, a reduction included; the 8 by 7 nest, one value of `j`
    ! more, is split into chunks.
    implicit none
    integer :: i, j, s, e
    integer :: a(8, 6), b(4, 5), c(8, 7)

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

    e = 0
    do j = 1, 7
        do i = 1, 8
            if (j <= 6) then
                if (a(i, j) /= 10 * i + j) error stop 1
                e = e + a(i, j)
            end if
            if (c(i, j) /= i - j) error stop 2
        end do
    end do
    do j = 1, 5
        do i = 1, 4
            if (b(i, j) /= i * j) error stop 3
        end do
    end do
    if (s /= e) error stop 4
    print *, "ok"
end program
