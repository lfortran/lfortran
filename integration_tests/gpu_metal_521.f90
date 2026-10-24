program gpu_metal_521
    ! The intermediate array is a target, and a statement of the time loop
    ! reads it through a pointer: the loops must not be fused, since the
    ! fused loop does not store the array.
    implicit none
    real, allocatable :: q(:), r(:)
    real, allocatable, target :: f(:)
    real, pointer :: p(:)
    real :: total, expected(8)
    integer :: n, s, i
    n = 8
    allocate(q(n), f(n), r(n))
    p => f
    do i = 1, n
        q(i) = real(i*i)
    end do
    r = 0.0
    total = 0.0
    do s = 1, 3
        do concurrent (i = 1:n)
            f(i) = 2.0 * q(i) + real(s)
        end do
        do concurrent (i = 2:n - 1)
            r(i) = r(i) + f(i - 1) + f(i + 1)
        end do
        total = total + p(3)
    end do
    expected = [0.0, 72.0, 132.0, 216.0, 324.0, 456.0, 612.0, 0.0]
    print *, total
    print *, r
    ! 19 + 20 + 21
    if (abs(total - 60.0) > 1e-3) error stop
    if (any(abs(r - expected) > 1e-3)) error stop
end program
