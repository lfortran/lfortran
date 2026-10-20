! Time skewing of parallel loops whose rows are beyond 2**30, and of
! parallel loops whose rows do not fit in the kind of the time loop's
! index: every row of every launch runs.
program gpu_metal_533
    implicit none
    integer, parameter :: lb = 2**30 - 2000, ub = lb + 5377
    integer, parameter :: lb8 = 1000, ub8 = lb8 + 5377
    real :: u(lb:ub), d(lb:ub), r(lb:ub), e(lb:ub)
    real :: v(lb8:ub8), f(lb8:ub8), s(lb8:ub8), g(lb8:ub8)
    integer :: it, k, j
    integer(1) :: it1
    do k = lb, ub
        u(k) = sin(0.01 * (k - lb)) + 0.1 * mod(k, 5)
    end do
    r = u
    d = 0; e = 0
    do it = 1, 6
        do concurrent (k = lb + 1:ub - 1)
            d(k) = u(k - 1) - 2.0 * u(k) + u(k + 1)
        end do
        do concurrent (k = lb + 1:ub - 1)
            u(k) = u(k) + 0.2 * d(k)
        end do
    end do
    do it = 1, 6
        do k = lb + 1, ub - 1
            e(k) = r(k - 1) - 2.0 * r(k) + r(k + 1)
        end do
        do k = lb + 1, ub - 1
            r(k) = r(k) + 0.2 * e(k)
        end do
    end do
    print *, maxval(abs(u - r))
    if (maxval(abs(u - r)) > 1e-5) error stop 1

    do j = lb8, ub8
        v(j) = sin(0.01 * (j - lb8)) + 0.1 * mod(j, 5)
    end do
    s = v
    f = 0; g = 0
    do it1 = 1_1, 6_1
        do concurrent (j = lb8 + 1:ub8 - 1)
            f(j) = v(j - 1) - 2.0 * v(j) + v(j + 1)
        end do
        do concurrent (j = lb8 + 1:ub8 - 1)
            v(j) = v(j) + 0.2 * f(j)
        end do
    end do
    do it = 1, 6
        do j = lb8 + 1, ub8 - 1
            g(j) = s(j - 1) - 2.0 * s(j) + s(j + 1)
        end do
        do j = lb8 + 1, ub8 - 1
            s(j) = s(j) + 0.2 * g(j)
        end do
    end do
    print *, maxval(abs(v - s))
    if (maxval(abs(v - s)) > 1e-5) error stop 2
    print *, "ok"
end program
