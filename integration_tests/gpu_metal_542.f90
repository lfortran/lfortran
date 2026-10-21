program gpu_metal_542
    ! The inner DO loop around the pair ends at a component the outer loop
    ! sets to 0 before its last iteration: the inner loop runs no
    ! iteration there, so the intermediate array must be stored earlier.
    implicit none
    type :: cfg_t
        integer :: nt
    end type
    type(cfg_t) :: c
    real, allocatable :: q(:), f(:), r(:)
    real :: expected_f(8), expected_r(8)
    integer :: n, s, t, i
    n = 8
    allocate(q(n), f(n), r(n))
    f = -1.0
    r = 0.0
    do i = 1, n
        q(i) = real(i)
    end do
    c%nt = 2
    do s = 1, 3
        if (s == 3) c%nt = 0
        do t = 1, c%nt
            do concurrent (i = 1:n)
                f(i) = q(i) + 10 * s + t
            end do
            do concurrent (i = 2:n)
                r(i) = r(i) + f(i) + f(i - 1)
            end do
        end do
    end do
    expected_f = [23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0]
    expected_r = [0.0, 144.0, 152.0, 160.0, 168.0, 176.0, 184.0, 192.0]
    print *, f
    print *, r
    if (any(abs(f - expected_f) > 1e-3)) error stop
    if (any(abs(r - expected_r) > 1e-3)) error stop
end program
