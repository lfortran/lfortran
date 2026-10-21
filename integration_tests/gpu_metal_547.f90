program gpu_metal_547
    ! Statements of the time loop change a loop bound through another name
    ! of its storage. The producer's bound, through a pointer and through an
    ! EQUIVALENCE: the first time step writes more elements of f than the
    ! last, which the program sees afterwards. The consumer's bound, through
    ! a pointer: the consumer, which updates the array the producer reads,
    ! writes fewer elements in later time steps.
    implicit none
    real, allocatable :: q(:), f(:)
    real :: expected_f(8), expected_q(8)
    integer, target :: m
    integer, pointer :: mp
    integer :: n, s, i, k, k2
    equivalence (k, k2)
    n = 8
    allocate(q(n), f(n))
    expected_f = [1.0, 5.0, 5.0, 4.0, 5.0, 6.0, 7.0, -1.0]

    f = -1.0
    q = 0.0
    m = 7
    mp => m
    do s = 1, 3
        do concurrent (i = 1:m)
            f(i) = q(i) + real(i)
        end do
        do concurrent (i = 2:m)
            q(i) = q(i) + f(i) - f(i - 1)
        end do
        mp = 3
    end do
    print *, f
    if (m /= 3) error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop

    f = -1.0
    q = 0.0
    k2 = 7
    do s = 1, 3
        do concurrent (i = 1:k2)
            f(i) = q(i) + real(i)
        end do
        do concurrent (i = 2:k2)
            q(i) = q(i) + f(i) - f(i - 1)
        end do
        k = 3
    end do
    print *, f
    if (k2 /= 3) error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop

    f = -1.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    m = 0
    mp => m
    do s = 1, 3
        do concurrent (i = 1:n)
            f(i) = 0.5 * q(i) + real(i)
        end do
        do concurrent (i = 2:n - m)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        mp = 4
    end do
    expected_q = [1.0, 6.119140625, 11.712890625, 19.46875, 26.375, &
        37.625, 50.875, 66.125]
    print *, q
    if (m /= 4) error stop
    if (any(abs(q - expected_q) > 1e-4)) error stop
end program
