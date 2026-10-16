! An interface assignment(=) declared in the specification part of a main
! program, whose specific is an internal procedure of the program.
! See https://github.com/lfortran/lfortran/issues/13976

module defined_assignment_03_mod
    implicit none
    private
    public :: b_t, assignment(=)

    type :: b_t
        real :: x
    end type

    interface assignment(=)
        module procedure set_b
    end interface

contains

    subroutine set_b(lhs, rhs)
        type(b_t), intent(out) :: lhs
        real, intent(in) :: rhs
        lhs%x = 2*rhs
    end subroutine

end module

subroutine defined_assignment_03_negate(lhs, rhs)
    use defined_assignment_03_mod, only: b_t
    implicit none
    type(b_t), intent(out) :: lhs
    type(b_t), intent(in) :: rhs
    lhs%x = -rhs%x
end subroutine

! An earlier external procedure with an assignment(=) of its own.
subroutine defined_assignment_03_ext()
    use defined_assignment_03_mod, only: b_t
    implicit none
    interface assignment(=)
        subroutine defined_assignment_03_negate(lhs, rhs)
            import :: b_t
            type(b_t), intent(out) :: lhs
            type(b_t), intent(in) :: rhs
        end subroutine
    end interface
end subroutine

program defined_assignment_03
    use defined_assignment_03_mod
    implicit none

    type :: a_t
        integer :: i
    end type

    interface assignment(=)
        procedure set_a
    end interface

    type(a_t) :: a
    type(b_t) :: b, b2

    a = 5
    if (a%i /= 5) error stop

    ! The use associated defined assignment stays accessible.
    b = 1.5
    if (abs(b%x - 3.0) > 1e-6) error stop

    call assign_in_internal()

    ! Intrinsic: the assignment(=) of defined_assignment_03_ext() and of
    ! local_assignment() is not the program's.
    b2 = b
    if (abs(b2%x - 3.0) > 1e-6) error stop

    print *, a%i, b%x, b2%x

contains

    subroutine set_a(lhs, rhs)
        type(a_t), intent(out) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs
    end subroutine

    subroutine assign_in_internal()
        type(a_t) :: c
        c = 7
        if (c%i /= 7) error stop
    end subroutine

    subroutine local_assignment()
        interface assignment(=)
            subroutine defined_assignment_03_negate(lhs, rhs)
                import :: b_t
                type(b_t), intent(out) :: lhs
                type(b_t), intent(in) :: rhs
            end subroutine
        end interface
    end subroutine

end program
