module gpu_metal_543_m
    implicit none
    type :: state
        real, allocatable :: f(:)
    end type
contains
    pure real function third(t)
        type(state), intent(in) :: t
        third = t%f(3)
    end function
end module

program gpu_metal_543
    ! The intermediate array is a component, which other statements of
    ! the DO loop around the pair read through the whole derived-type
    ! variable: a function argument and a structure assignment.
    use gpu_metal_543_m
    implicit none
    type(state) :: g, h, u
    real, allocatable :: q(:), r(:), p(:)
    real :: total, total2
    integer :: n, s, i
    n = 8
    allocate(q(n), r(n), p(n), g%f(n), u%f(n))
    g%f = 0.0
    u%f = 0.0
    r = 0.0
    p = 0.0
    do i = 1, n
        q(i) = real(i)
    end do
    total = 0.0
    total2 = 0.0
    do s = 1, 4
        do concurrent (i = 1:n)
            g%f(i) = q(i) + 10 * s
        end do
        do concurrent (i = 2:n)
            r(i) = r(i) + g%f(i) + g%f(i - 1)
        end do
        total = total + third(g)
    end do
    do s = 1, 4
        do concurrent (i = 1:n)
            u%f(i) = q(i) + 10 * s
        end do
        do concurrent (i = 2:n)
            p(i) = p(i) + u%f(i) + u%f(i - 1)
        end do
        h = u
        total2 = total2 + h%f(3)
    end do
    print *, total, total2
    print *, r
    if (abs(total - 112.0) > 1e-3) error stop
    if (abs(total2 - 112.0) > 1e-3) error stop
    if (abs(r(2) - 212.0) > 1e-3) error stop
    if (abs(r(8) - 260.0) > 1e-3) error stop
    if (any(abs(p - r) > 1e-3)) error stop
end program
