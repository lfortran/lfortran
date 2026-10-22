! The DO variables of a skewed nest of time loops hold, after the nest,
! the values the same nest without parallel loops leaves in them.
program gpu_metal_534
    implicit none
    integer, parameter :: n = 5003
    real :: u(n), d(n), r(n), e(n)
    integer :: it, s, k, it2, s2
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    r = u
    do it = 1, 4
        do s = 1, 3
            do concurrent (k = 2:n - 1)
                d(k) = u(k - 1) - 2.0 * u(k) + u(k + 1)
            end do
            do concurrent (k = 2:n - 1)
                u(k) = u(k) + 0.2 * d(k)
            end do
        end do
    end do
    do it2 = 1, 4
        do s2 = 1, 3
            do k = 2, n - 1
                e(k) = r(k - 1) - 2.0 * r(k) + r(k + 1)
            end do
            do k = 2, n - 1
                r(k) = r(k) + 0.2 * e(k)
            end do
        end do
    end do
    print *, maxval(abs(u - r)), it, s, it2, s2
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    if (it /= it2 .or. s /= s2) error stop 2
    print *, "ok"
end program
