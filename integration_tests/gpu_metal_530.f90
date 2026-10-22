! Time skewing of an upwind (one-sided) update: the stencil reads the row
! behind the one it writes, so the next step must not overwrite a row
! before the previous step has read it.
program gpu_metal_530
    implicit none
    integer, parameter :: n = 5003
    real :: u(n), d(n), r(n), e(n)
    integer :: it, k
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    r = u
    do it = 1, 6
        do concurrent (k = 2:n)
            d(k) = u(k) - 0.5 * (u(k) - u(k - 1))
        end do
        do concurrent (k = 2:n)
            u(k) = d(k)
        end do
    end do
    do it = 1, 6
        do k = 2, n
            e(k) = r(k) - 0.5 * (r(k) - r(k - 1))
        end do
        do k = 2, n
            r(k) = e(k)
        end do
    end do
    print *, maxval(abs(u - r))
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    ! A read three rows behind, in one parallel loop: u(k - 3) is read
    ! before the update of row k - 3 in the same step.
    r = u
    do it = 1, 5
        do concurrent (k = 4:n)
            d(k) = 0.25 * (u(k) + 3.0 * u(k - 3))
        end do
        do concurrent (k = 4:n)
            u(k) = d(k)
        end do
    end do
    do it = 1, 5
        do k = 4, n
            e(k) = 0.25 * (r(k) + 3.0 * r(k - 3))
        end do
        do k = 4, n
            r(k) = e(k)
        end do
    end do
    print *, maxval(abs(u - r))
    if (maxval(abs(u - r)) > 1e-5) error stop 2
    print *, "ok"
end program
