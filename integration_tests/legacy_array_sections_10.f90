program legacy_array_sections_10
    implicit none

    real :: a(4)

    a = [1.0, 2.0, 3.0, 4.0]
    call sub(a(2))

    if (abs((a(2) + a(3) + a(4)) - 9.0) > 1e-6) error stop
end program legacy_array_sections_10

subroutine sub(x)
    implicit none
    real, intent(inout) :: x(3)

    x(1) = 2.0
    x(2) = 3.0
    x(3) = 4.0
end subroutine sub
