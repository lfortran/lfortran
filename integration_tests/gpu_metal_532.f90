! Time skewing of a time loop inside a BLOCK whose array the parallel
! loops write and read: the array is shared by the loops, not private to
! an iteration.
program gpu_metal_532
    implicit none
    integer, parameter :: n = 5003
    real :: u(n), r(n), e(n)
    integer :: it, k
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    r = u
    block
        real :: w(n)
        do it = 1, 6
            do concurrent (k = 1:n)
                w(k) = u(k)
            end do
            do concurrent (k = 1:n - 3)
                u(k) = 0.5 * (w(k) + w(k + 3))
            end do
        end do
    end block
    do it = 1, 6
        do k = 1, n
            e(k) = r(k)
        end do
        do k = 1, n - 3
            r(k) = 0.5 * (e(k) + e(k + 3))
        end do
    end do
    print *, maxval(abs(u - r))
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    print *, "ok"
end program
