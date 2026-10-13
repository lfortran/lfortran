! A parent type declares a type-bound generic assignment and an extended
! type overrides the specific binding. The assignment calls the binding of
! the object's own type.
! See https://github.com/lfortran/lfortran/issues/14854

module defined_assignment_override_01_mod
    implicit none

    type :: base_t
        integer :: i = 0
    contains
        procedure :: set
        generic :: assignment(=) => set
    end type

    type, extends(base_t) :: child_t
    contains
        procedure :: set => set_child
    end type

    type, extends(child_t) :: grandchild_t
    end type

contains

    subroutine set(lhs, rhs)
        class(base_t), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs
    end subroutine

    subroutine set_child(lhs, rhs)
        class(child_t), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = -rhs
    end subroutine

end module

program defined_assignment_override_01
    use defined_assignment_override_01_mod
    implicit none
    type(child_t) :: c
    type(grandchild_t) :: g

    c = 5
    if (c%i /= -5) error stop
    g = 7
    if (g%i /= -7) error stop
    print *, c%i, g%i
end program
