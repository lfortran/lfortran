program gpu_metal_688
    ! Offloaded loops whose iterations run in several launches of the
    ! kernel (lfortran/lfortran#16569), on every device that offloads them,
    ! Metal included (lfortran/lfortran#16541), with a scalar the host reads
    ! after the loop and with loops inside an iteration that a device may
    ! share out among several threads per iteration (its lanes). The test is
    ! compiled with --gpu-chunk-limit=4: loops of a few dozen iterations run
    ! in chunks, each of them on all the lanes of its iterations.
    implicit none
    integer, parameter :: w = 6
    real :: a(w), d(29, w), e(w, 9, 2)
    integer :: i, j, k, n, m, p, last, kk

    do j = 1, w
        a(j) = real(10 * j)
    end do

    ! A private array of run-time size, and two loops over it inside each
    ! iteration, which the lanes of the iteration share.
    n = 29
    m = w
    d = 0
    do concurrent (i = 1:n)
        block
            real :: t(m)
            do j = 1, m
                t(j) = a(j) + real(i)
            end do
            do j = 1, m
                d(i, j) = t(m + 1 - j)
            end do
        end block
    end do
    do j = 1, w
        do i = 1, 29
            if (d(i, j) /= a(w + 1 - j) + real(i)) error stop 1
        end do
    end do

    ! A scalar that the last iteration assigns.
    last = -1
    do concurrent (i = 1:n)
        if (i == n) last = 7 * i
    end do
    if (last /= 7 * n) error stop 2

    ! A nest with an array statement in each iteration.
    p = 9
    e = 0
    do concurrent (i = 1:p, k = 1:2)
        e(:, i, k) = a + real(100 * i + k)
    end do
    do k = 1, 2
        do i = 1, 9
            do j = 1, w
                if (e(j, i, k) /= a(j) + real(100 * i + k)) error stop 3
            end do
        end do
    end do

    ! A scalar every iteration assigns and then reads: each iteration
    ! reads its own value, and the host the one of the last iteration.
    kk = 5
    last = -1
    do concurrent (i = 1:n)
        kk = i * 10
        d(i, 1) = real(kk + 1)
        if (i == n) last = kk + i
    end do
    do i = 1, n
        if (d(i, 1) /= real(10 * i + 1)) error stop 4
    end do
    if (last /= 10 * n + n) error stop 5
    print *, "ok"
end program
