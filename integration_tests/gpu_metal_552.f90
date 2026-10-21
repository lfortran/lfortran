program gpu_metal_552
    ! The variable of an implied DO in a PRINT of the time loop is the
    ! producer's bound `m`. With --use-loop-variable-after-loop it keeps
    ! its final value after the statement, as the standard requires. The
    ! consumer updates the field the producer reads (the array is
    ! renamed).
    implicit none
    real, allocatable :: q(:), f(:)
    real :: expected_f(8), expected_q(8)
    integer :: n, m, s, i
    n = 8
    m = 8
    allocate(q(n), f(n))
    f = -1.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    do s = 1, 3
        do concurrent (i = 1:m - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:m - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        print '(i0)', (s, m = 4, 4)
    end do
    expected_f = [3.875, 5.125, 7.0, 8.5, 11.0, 13.0, 15.0, -1.0]
    expected_q = [1.0, 5.1875, 10.46875, 17.375, 25.5, 36.5, 49.5, 64.0]
    print *, f
    print *, q
    if (m /= 5) error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop
    if (any(abs(q - expected_q) > 1e-5)) error stop
end program
