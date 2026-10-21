program gpu_metal_548
    ! The end of the time loop reads the loop's own variable. It is
    ! evaluated once, when the loop starts, so the loop runs three
    ! iterations; the variable then never equals the expression's current
    ! value. The values of the last iteration must still be stored. Then
    ! the end is a pointer whose target the loop changes.
    implicit none
    real, allocatable :: q(:), f(:)
    real :: expected_f(8), expected_q(8)
    integer :: n, s, i
    integer, target :: ns
    integer, pointer :: np
    n = 8
    allocate(q(n), f(n))
    f = -1.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    s = 1
    do s = 1, s + 2
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    expected_f = [3.875, 5.125, 7.0, 9.0, 11.0, 12.875, 14.125, -1.0]
    expected_q = [1.0, 5.1875, 10.46875, 17.5, 26.5, 37.46875, 50.1875, 64.0]
    print *, f
    print *, q
    if (any(abs(f - expected_f) > 1e-4)) error stop
    if (any(abs(q - expected_q) > 1e-4)) error stop

    f = -1.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    ns = 3
    np => ns
    do s = 1, np
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        ns = 1
    end do
    print *, f
    print *, q
    if (np /= 1) error stop
    if (any(abs(f - expected_f) > 1e-4)) error stop
    if (any(abs(q - expected_q) > 1e-4)) error stop
end program
