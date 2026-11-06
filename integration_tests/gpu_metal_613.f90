program gpu_metal_613
    ! Offloaded loops that run in several launches of their kernels
    ! (lfortran/lfortran#16569) and hand arrays from one to the next: a
    ! later loop reads what the chunks of an earlier one wrote, an array
    ! one loop writes whole before reading it (which need not be copied to
    ! the device first) keeps what each chunk wrote, and a loop with a
    ! private array of run-time size gives every thread of a chunk its
    ! own. The test is compiled with --gpu-chunk-limit=5.
    implicit none
    integer, parameter :: n = 37
    real :: a(n), b(n), c(n), d(n, 4)
    integer :: i, j, m

    do concurrent (i = 1:n)
        a(i) = real(i)
    end do
    ! Every element of `b` is written before it is read.
    b = -1
    do concurrent (i = 1:n)
        b(i) = 2 * a(i)
    end do
    do concurrent (i = 1:n)
        c(i) = a(i) + b(i)
    end do
    do concurrent (i = 1:n)
        a(i) = c(n + 1 - i)
    end do
    do i = 1, n
        if (b(i) /= 2 * i) error stop 1
        if (c(i) /= 3 * i) error stop 2
        if (a(i) /= 3 * (n + 1 - i)) error stop 3
    end do

    ! A private array whose extent is known at run time only.
    m = 4
    d = 0
    do concurrent (i = 1:n)
        block
            real :: t(m)
            do j = 1, m
                t(j) = b(i) + j
            end do
            do j = 1, m
                d(i, j) = t(m + 1 - j)
            end do
        end block
    end do
    do j = 1, 4
        do i = 1, n
            if (d(i, j) /= 2 * i + (m + 1 - j)) error stop 4
        end do
    end do
    print *, "ok"
end program
