! An interface assignment(=) declared in a procedure is built in that procedure.
! See https://github.com/lfortran/lfortran/issues/14755

module defined_assignment_04_mod
    implicit none

    type :: a_t
        integer :: i
    end type

    interface assignment(=)
        module procedure set_from_real
    end interface

contains

    subroutine set_from_real(lhs, rhs)
        type(a_t), intent(out) :: lhs
        real, intent(in) :: rhs
        lhs%i = int(rhs)
    end subroutine

    ! A module procedure: its block extends the module's assignment(=).
    subroutine module_proc()
        interface assignment(=)
            subroutine defined_assignment_04_double(lhs, rhs)
                import :: a_t
                type(a_t), intent(out) :: lhs
                integer, intent(in) :: rhs
            end subroutine
        end interface
        type(a_t) :: a
        a = 5
        if (a%i /= 10) error stop
        a = 7.0
        if (a%i /= 7) error stop
    end subroutine

end module

subroutine defined_assignment_04_double(lhs, rhs)
    use defined_assignment_04_mod, only: a_t
    implicit none
    type(a_t), intent(out) :: lhs
    integer, intent(in) :: rhs
    lhs%i = 2*rhs
end subroutine

! An external procedure, whose specific is its internal procedure.
subroutine defined_assignment_04_ext()
    use defined_assignment_04_mod, only: a_t
    implicit none
    interface assignment(=)
        procedure set_triple
    end interface
    type(a_t) :: a
    a = 5
    if (a%i /= 15) error stop
contains
    subroutine set_triple(lhs, rhs)
        type(a_t), intent(out) :: lhs
        integer, intent(in) :: rhs
        lhs%i = 3*rhs
    end subroutine
end subroutine

program defined_assignment_04
    use defined_assignment_04_mod
    implicit none
    call module_proc()
    call defined_assignment_04_ext()
    call internal_proc()
    print *, "ok"
contains
    ! An internal procedure of a main program.
    subroutine internal_proc()
        interface assignment(=)
            subroutine defined_assignment_04_double(lhs, rhs)
                import :: a_t
                type(a_t), intent(out) :: lhs
                integer, intent(in) :: rhs
            end subroutine
        end interface
        type(a_t) :: a
        a = 4
        if (a%i /= 8) error stop
    end subroutine
end program
