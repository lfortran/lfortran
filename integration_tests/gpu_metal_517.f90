program gpu_metal_517
    ! The producer's bounds depend on the variable of the DO loop around
    ! the pair: elements written only in earlier steps must still be
    ! written.
    implicit none
    real, allocatable :: q(:), f(:), r(:)
    real :: expected_f(10), expected_r(10)
    integer :: n, s, i
    n = 10
    allocate(q(n), f(n), r(n))
    do i = 1, n
        q(i) = real(i)
    end do
    f = -100.0
    r = 0.0
    do s = 1, 3
        do concurrent (i = 1:n - s)
            f(i) = 2.0 * q(i) + real(s)
        end do
        do concurrent (i = 2:n - s - 1)
            r(i) = r(i) + f(i - 1) + f(i + 1)
        end do
    end do
    expected_f = [5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 18.0, 19.0, -100.0]
    expected_r = [0.0, 36.0, 48.0, 60.0, 72.0, 84.0, 62.0, 34.0, 0.0, 0.0]
    print *, f
    print *, r
    if (any(abs(f - expected_f) > 1e-3)) error stop
    if (any(abs(r - expected_r) > 1e-3)) error stop
end program
