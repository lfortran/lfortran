program gpu_metal_561
    ! The intermediate array f is in EQUIVALENCE with h. Another statement
    ! of the time loop, and the consumer itself, read f's values through h,
    ! so f must be stored in every time step.
    implicit none
    real :: q(8), f(8), g(8), h(8)
    equivalence (h, f)
    real :: acc
    real :: expected_g(8)
    integer :: n, s, i
    n = 8

    f = -1.0
    g = 0.0
    acc = 0.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    do s = 1, 3
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            g(i) = 0.25 * (f(i) - f(i - 1))
        end do
        acc = acc + h(3)
        q = 2.0 * q
    end do
    print *, acc
    if (abs(acc - 49.0) > 1e-4) error stop

    f = -1.0
    g = 0.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    do s = 1, 3
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            g(i) = g(i) + 0.25 * (f(i) - f(i - 1)) + h(i)
        end do
        q = 2.0 * q
    end do
    print *, g
    expected_g = [0.0, 38.5, 52.5, 66.5, 80.5, 94.5, 108.5, 0.0]
    if (any(abs(g - expected_g) > 1e-4)) error stop
end program
