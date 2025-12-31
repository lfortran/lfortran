program legacy_array_sections_10
    implicit none

    real, allocatable :: a(:)

    allocate(a(4))
    a = [1.0, 2.0, 3.0, 4.0]

    call foo(a)
    call foo(a(2))

    if (abs(a(1) - 11.0) > 1e-6) error stop
    if (abs(a(2) - 12.0) > 1e-6) error stop
end program legacy_array_sections_10
