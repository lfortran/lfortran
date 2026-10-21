program gpu_metal_551
    ! An IOSTAT= specifier of a WRITE inside a BLOCK of the time loop
    ! changes the producer's bound `m`. The consumer updates the field the
    ! producer reads (the array is renamed). The first time step writes
    ! more elements of f than the others, which the program sees
    ! afterwards.
    implicit none
    real, allocatable :: q(:), f(:)
    real :: expected_f(8), expected_q(8)
    character(len=8) :: str
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
        block
            write(str, '(i0)', iostat=m) s
        end block
    end do
    expected_f = [3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, -1.0]
    expected_q = [1.0, 4.5, 9.5, 16.5, 25.5, 36.5, 49.5, 64.0]
    print *, f
    print *, q
    if (m /= 0 .or. trim(str) /= '3') error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop
    if (any(abs(q - expected_q) > 1e-5)) error stop
end program
