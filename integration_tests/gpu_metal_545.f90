program gpu_metal_545
    ! The producer reads a scalar `j` of the program, and the consumer's
    ! index is also named `j`: the fused loop must still read the scalar.
    implicit none
    real, allocatable :: q(:), r(:), f(:)
    real :: expected_r(8)
    integer :: n, s, i, j
    n = 8
    allocate(q(n), r(n), f(n))
    f = 0.0
    r = 0.0
    do i = 1, n
        q(i) = real(i)
    end do
    do s = 1, 3
        j = 100
        do concurrent (i = 1:n)
            f(i) = q(i) + j
        end do
        do concurrent (j = 2:n)
            r(j) = r(j) + f(j) + f(j - 1)
        end do
    end do
    expected_r = [0.0, 609.0, 615.0, 621.0, 627.0, 633.0, 639.0, 645.0]
    print *, f
    print *, r
    if (abs(f(1) - 101.0) > 1e-3) error stop
    if (abs(f(8) - 108.0) > 1e-3) error stop
    if (any(abs(r - expected_r) > 1e-3)) error stop
end program
