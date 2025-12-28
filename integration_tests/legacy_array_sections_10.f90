program legacy_array_sections_10
    implicit none

    real, target :: a(4)

    a = [1.0, 2.0, 3.0, 4.0]
    call sub(a(2))

contains

    subroutine sub(x)
        real, pointer :: x(:)

        if (size(x) /= 3) error stop
        if (abs(sum(x) - 9.0) > 1e-6) error stop
    end subroutine sub

end program legacy_array_sections_10
