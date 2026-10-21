module gpu_metal_549_mod
    implicit none
contains
    subroutine set_to_four(k)
        integer, intent(inout) :: k
        k = 4
    end subroutine
end module

program gpu_metal_549
    ! A subroutine called inside a BLOCK of the time loop changes the
    ! producer's bound `m`. The first time step writes more elements of f
    ! than the last, which the program sees afterwards.
    use gpu_metal_549_mod
    implicit none
    real, allocatable :: q(:), f(:), g(:)
    real :: expected_f(8), expected_g(8)
    integer :: n, m, s, i
    n = 8
    m = 8
    allocate(q(n), f(n), g(n))
    f = -1.0
    g = 0.0
    do i = 1, n
        q(i) = real(i*i)
    end do
    do s = 1, 3
        do concurrent (i = 1:m - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:m - 1)
            g(i) = 0.25 * (f(i) - f(i - 1))
        end do
        block
            call set_to_four(m)
        end block
    end do
    expected_f = [3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, -1.0]
    expected_g = [0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0]
    print *, f
    print *, g
    if (m /= 4) error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop
    if (any(abs(g - expected_g) > 1e-5)) error stop
end program
