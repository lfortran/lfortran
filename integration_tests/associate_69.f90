program associate_69
    implicit none
    real :: x
    real(8) :: y
    x = 1.0
    y = 1.0d0
    associate (a => x, b => y)
        call check4(a + 0.5)
        call check8(b + 0.5d0)
    end associate
contains
    subroutine check4(v)
        real, intent(in) :: v
        if (abs(v - 1.5) > 1.0e-6) error stop
    end subroutine
    subroutine check8(v)
        real(8), intent(in) :: v
        if (abs(v - 1.5d0) > 1.0d-12) error stop
    end subroutine
end program
