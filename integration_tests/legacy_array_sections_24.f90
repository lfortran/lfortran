module legacy_array_sections_24_sizes
    implicit none
    integer :: n
    integer :: m
end module

module legacy_array_sections_24_mod
    implicit none
contains
    subroutine caller(a)
        use legacy_array_sections_24_sizes
        real :: a(0:n)
        call fill(a(0))
        call fill_tail(a(2))
    end subroutine

    subroutine fill(b)
        use legacy_array_sections_24_sizes
        real :: b(0:n)
        integer :: i
        do i = 0, n
            b(i) = real(i)
        end do
    end subroutine

    subroutine fill_tail(c)
        use legacy_array_sections_24_sizes
        real :: c(m)
        c = -1.0
    end subroutine
end module

program legacy_array_sections_24
    use legacy_array_sections_24_sizes
    use legacy_array_sections_24_mod
    implicit none
    real :: x(0:5)
    integer :: i
    n = 5
    m = 3
    x = 100.0
    call caller(x)
    print *, x
    if (abs(x(0) - 0.0) > 1e-6) error stop
    if (abs(x(1) - 1.0) > 1e-6) error stop
    do i = 2, 4
        if (abs(x(i) + 1.0) > 1e-6) error stop
    end do
    if (abs(x(5) - 5.0) > 1e-6) error stop
end program
