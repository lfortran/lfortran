module gpu_metal_544_m
    implicit none
    type :: state
        real, allocatable :: q(:)
    end type
contains
    pure real function fourth(t)
        type(state), intent(in) :: t
        fourth = t%q(4)
    end function
end module

program gpu_metal_544
    ! The array the consumer updates is a component, which another
    ! statement of the DO loop around the pair reads through the whole
    ! derived-type variable: it must not take turns with a copy.
    use gpu_metal_544_m
    implicit none
    type(state) :: g
    real, allocatable :: f(:)
    real :: total
    real :: expected_q(8)
    integer :: n, s, i
    n = 8
    allocate(g%q(n), f(n))
    do i = 1, n
        g%q(i) = real(i*i)
    end do
    total = 0.0
    do s = 1, 4
        do concurrent (i = 1:n - 1)
            f(i) = g%q(i + 1) - g%q(i)
        end do
        do concurrent (i = 2:n - 1)
            g%q(i) = g%q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        total = total + fourth(g)
    end do
    expected_q = [1.0, 5.4609375, 10.90625, 17.9921875, 26.9921875, &
        37.90625, 50.4609375, 64.0]
    print *, total
    print *, g%q
    if (abs(total - 68.9921875) > 1e-3) error stop
    if (any(abs(g%q - expected_q) > 1e-3)) error stop
end program
