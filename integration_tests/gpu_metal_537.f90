! Time skewing of a time loop whose second parallel loop takes an inner
! bound from an array the first one writes: a replay must not see the
! bound of a later step, so the loop is not skewed.
program gpu_metal_537
    implicit none
    integer, parameter :: n = 4000, nt = 6
    integer :: cnt(n), v(nt, n), w(nt, n)
    integer :: k, j, it
    cnt = 0
    v = 0
    w = 0
    do it = 1, nt
        do concurrent (k = 1:n)
            cnt(k) = it
        end do
        do concurrent (k = 1:n, j = 1:cnt(1))
            v(j, k) = v(j, k) + 1
        end do
    end do
    do it = 1, nt
        do k = 1, n
            cnt(k) = it
        end do
        do k = 1, n
            do j = 1, cnt(1)
                w(j, k) = w(j, k) + 1
            end do
        end do
    end do
    if (any(v /= w)) error stop 1
    print *, "ok"
end program
