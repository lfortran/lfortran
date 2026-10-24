! Two modules each bind a private procedure named `assign` to the generic
! assignment of their own type. Each assignment calls the procedure of its
! own type.

module defined_assignment_same_name_01_a
    implicit none
    private
    public :: a_t

    type :: a_t
        integer :: i = 0
    contains
        procedure :: assign
        generic :: assignment(=) => assign
    end type

contains

    subroutine assign(lhs, rhs)
        class(a_t), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs
    end subroutine

end module

module defined_assignment_same_name_01_b
    implicit none
    private
    public :: b_t

    type :: b_t
        integer :: i = 0
    contains
        procedure :: assign
        generic :: assignment(=) => assign
    end type

contains

    subroutine assign(lhs, rhs)
        class(b_t), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = -rhs
    end subroutine

end module

program defined_assignment_same_name_01
    use defined_assignment_same_name_01_a
    use defined_assignment_same_name_01_b
    implicit none
    type(a_t) :: a
    type(b_t) :: b

    a = 1
    b = 2
    if (a%i /= 1) error stop
    if (b%i /= -2) error stop
    print *, a%i, b%i
end program
