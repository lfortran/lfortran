program gpu_metal_540
    ! The DO loop around the pair ends at an array element the loop
    ! changes: its iteration count is fixed on entry, so a test for its
    ! last iteration must not read the element again. The second loop
    ! changes the element through an ASSOCIATE name.
    implicit none
    real, allocatable :: q(:), f(:)
    real :: expected_f(8), expected_q(8)
    integer :: ns(1)
    integer :: n, s, i
    n = 8
    allocate(q(n), f(n))
    f = -1.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    ns(1) = 3
    do s = 1, ns(1)
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        ns(1) = 1
    end do
    expected_f = [3.875, 5.125, 7.0, 9.0, 11.0, 12.875, 14.125, -1.0]
    expected_q = [1.0, 5.1875, 10.46875, 17.5, 26.5, 37.46875, 50.1875, 64.0]
    print *, f
    print *, q
    if (any(abs(f - expected_f) > 1e-3)) error stop
    if (any(abs(q - expected_q) > 1e-3)) error stop

    f = -1.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    ns(1) = 3
    do s = 1, ns(1)
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        associate (m => ns(1))
            m = 1
        end associate
    end do
    print *, f
    print *, q
    if (any(abs(f - expected_f) > 1e-3)) error stop
    if (any(abs(q - expected_q) > 1e-3)) error stop
end program
