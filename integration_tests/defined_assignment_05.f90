! An interface assignment(=) declared in a procedure with an ENTRY statement.
! See https://github.com/lfortran/lfortran/issues/14755

module defined_assignment_05_mod
    implicit none

    type :: b_t
        real :: x
    end type

contains

    subroutine get_neg(r)
        real, intent(out) :: r
        interface assignment(=)
            subroutine defined_assignment_05_neg(lhs, rhs)
                import :: b_t
                type(b_t), intent(out) :: lhs
                type(b_t), intent(in) :: rhs
            end subroutine
        end interface
        type(b_t) :: p, q
        q%x = 3
        p = q
        r = p%x
        return
    entry get_neg_4(r)
        q%x = 4
        p = q
        r = p%x
    end subroutine

end module

subroutine defined_assignment_05_neg(lhs, rhs)
    use defined_assignment_05_mod, only: b_t
    implicit none
    type(b_t), intent(out) :: lhs
    type(b_t), intent(in) :: rhs
    lhs%x = -rhs%x
end subroutine

program defined_assignment_05
    use defined_assignment_05_mod
    implicit none
    real :: r
    call get_neg(r)
    if (abs(r + 3) > 1e-6) error stop
    call get_neg_4(r)
    if (abs(r + 4) > 1e-6) error stop
    print *, r
end program
