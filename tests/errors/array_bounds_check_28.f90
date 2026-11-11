module array_bounds_check_28_m
    implicit none
contains
    impure elemental subroutine esub(a, b)
        integer, intent(inout), optional :: a
        integer, intent(in), optional :: b
        if (present(a) .and. present(b)) a = a + b
    end subroutine

    subroutine run(x, y)
        integer, intent(inout) :: x(:)
        integer, intent(in), optional :: y(:)
        ! every dummy of esub is optional, so y must be present
        call esub(a=x, b=y)
    end subroutine
end module

program array_bounds_check_28
    use array_bounds_check_28_m
    implicit none
    integer :: x(2)
    x = [1, 2]
    call run(x)
    print *, "not reached"
end program
