module gpu_metal_554_mod
    implicit none
    integer :: m, k(3)
    equivalence (m, k(2))
end module

program gpu_metal_554
    ! The producer's bound `m` is in EQUIVALENCE with an element of the
    ! array `k` in a module, and the time loop assigns that element. The
    ! first time step writes more elements of f than the last, which the
    ! program sees afterwards.
    use gpu_metal_554_mod
    implicit none
    real, allocatable :: q(:), f(:), g(:)
    real :: expected_f(8), expected_g(8)
    integer :: n, s, i
    n = 8
    m = 8
    allocate(q(n), f(n), g(n))
    f = -1.0
    g = 0.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    do s = 1, 3
        do concurrent (i = 1:m - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:m - 1)
            g(i) = 0.25 * (f(i) - f(i - 1))
        end do
        k(2) = 4
    end do
    expected_f = [3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, -1.0]
    expected_g = [0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0]
    print *, f
    print *, g
    if (m /= 4) error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop
    if (any(abs(g - expected_g) > 1e-5)) error stop
end program
