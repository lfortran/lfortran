program gpu_metal_541
    ! The producer's bounds read a component the DO loop around the pair
    ! changes: elements written only in earlier steps must still be
    ! written.
    implicit none
    type :: grid
        integer :: n
    end type
    type(grid) :: g
    real, allocatable :: q(:), f(:), r(:)
    real :: expected_f(10), expected_r(10)
    integer :: n, s, i
    n = 10
    allocate(q(n), f(n), r(n))
    f = -100.0
    r = 0.0
    do i = 1, n
        q(i) = real(i)
    end do
    g%n = 8
    do s = 1, 4
        do concurrent (i = 1:g%n)
            f(i) = q(i) + 10 * s
        end do
        do concurrent (i = 2:g%n)
            r(i) = r(i) + f(i) + f(i - 1)
        end do
        g%n = g%n - 1
    end do
    expected_f = [41.0, 42.0, 43.0, 44.0, 45.0, 36.0, 27.0, 18.0, &
        -100.0, -100.0]
    expected_r = [0.0, 212.0, 220.0, 228.0, 236.0, 153.0, 86.0, 35.0, &
        0.0, 0.0]
    print *, f
    print *, r
    if (any(abs(f - expected_f) > 1e-3)) error stop
    if (any(abs(r - expected_r) > 1e-3)) error stop
end program
