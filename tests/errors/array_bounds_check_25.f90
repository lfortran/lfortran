module array_bounds_check_25_m
    implicit none
contains
    elemental real function f(x, y)
        real, intent(in) :: x
        real, intent(in), optional :: y
        f = x
        if (present(y)) f = x + y
    end function
    subroutine g(b)
        real, intent(in), optional :: b(:)
        print *, f(1.0, b)
    end subroutine
end module

program array_bounds_check_25
    ! An absent optional array is the only array actual of an elemental function
    use array_bounds_check_25_m
    implicit none
    call g()
end program
