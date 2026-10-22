program gpu_metal_515
    ! The producer writes an element only under a test whose normal form
    ! has more alternatives than the analysis keeps (seven conditions
    ! `i /= k`): the test cannot be proven for the elements the consumer
    ! reads, and the loops must not be fused.
    implicit none
    real, allocatable :: q(:), f(:), r(:)
    real :: expected(12)
    integer :: n, s, i
    n = 12
    allocate(q(n), f(n), r(n))
    do i = 1, n
        q(i) = real(i)
    end do
    f = -100.0
    r = 0.0
    do s = 1, 3
        do concurrent (i = 1:n)
            if (i /= 2 .and. i /= 3 .and. i /= 4 .and. i /= 5 .and. i /= 6 &
                    .and. i /= 7 .and. i /= 8) f(i) = q(i)
        end do
        do concurrent (i = 2:n - 1)
            r(i) = r(i) + f(i - 1) + f(i + 1)
        end do
    end do
    expected = [0.0, -297.0, -600.0, -600.0, -600.0, -600.0, -600.0, &
        -273.0, -270.0, 60.0, 66.0, 0.0]
    print *, r
    if (any(abs(r - expected) > 1e-3)) error stop
    if (any(abs(f(9:12) - q(9:12)) > 1e-3) .or. f(1) /= 1.0) error stop
    if (any(f(2:8) /= -100.0)) error stop
end program
