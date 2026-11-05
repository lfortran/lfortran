program array_bounds_check_24
    ! The array actuals of an elemental subroutine have different shapes
    implicit none
    real, allocatable :: a(:), b(:)
    allocate(a(2), b(3))
    a = 1
    b = 2
    call s(a, b)
    print *, a
contains
    elemental subroutine s(x, y)
        real, intent(inout) :: x
        real, intent(in) :: y
        x = x + y
    end subroutine
end program
