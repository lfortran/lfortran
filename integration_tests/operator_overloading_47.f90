! A unary `.not.` on an operand that is not logical calls the user-defined
! `operator(.not.)` (#15340).
module operator_overloading_47_m
    implicit none
    type :: t
        integer :: x
    end type
    interface operator(.not.)
        module procedure not_t, not_i
    end interface
contains
    logical function not_t(a)
        type(t), intent(in) :: a
        not_t = a%x == 0
    end function

    integer function not_i(i)
        integer, intent(in) :: i
        not_i = -i
    end function
end module

program operator_overloading_47
    use operator_overloading_47_m
    implicit none
    type(t) :: a
    a%x = 0
    if (.not. (.not. a)) error stop 1
    a%x = 1
    if (.not. a) error stop 2
    if ((.not. 5) /= -5) error stop 3
    print *, "OK"
end program
