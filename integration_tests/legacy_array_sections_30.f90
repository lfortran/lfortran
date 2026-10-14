program legacy_array_sections_30
    implicit none
    real :: x(3)
    x = 1.0
    call s(x)
    print *, x
    if (abs(x(1) - 1.0) > 1e-6) error stop
    if (abs(x(2) - 4.0) > 1e-6) error stop
    if (abs(x(3) - 1.0) > 1e-6) error stop
end program

subroutine s(x)
    implicit none
    real :: x
    dimension x(1)
    x(2) = 4.0
end subroutine
