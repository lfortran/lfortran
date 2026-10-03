module nested_vars_03_mod
    implicit none

    type :: linop_type
        procedure(vector_sub), nopass, pointer :: matvec => null()
    end type

    abstract interface
        subroutine vector_sub(x, y, alpha, beta, op)
            real, intent(in) :: x(:)
            real, intent(inout) :: y(:)
            real, intent(in) :: alpha
            real, intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine vector_sub
    end interface

contains

    subroutine call_matvec(obj, x, y, alpha, beta, op)
        type(linop_type), intent(in) :: obj
        real, intent(in) :: x(:)
        real, intent(inout) :: y(:)
        real, intent(in) :: alpha
        real, intent(in) :: beta
        character(1), intent(in) :: op

        call obj%matvec(x, y, alpha, beta, op)
    end subroutine call_matvec

    subroutine run_repro()
        call case_dispatch_via_non_nested_arg()
    end subroutine run_repro

    subroutine case_dispatch_via_non_nested_arg()
        type(linop_type) :: a
        real :: x(2), y(2)
        logical, target :: di(2)
        logical, pointer :: di_(:)

        x = [10.0, 20.0]
        y = [30.0, 40.0]
        di = [.false., .true.]

        di_ => di
        a%matvec => matvec
        call call_matvec(a, x, y, 1.0, 0.0, 'N')

        if (abs(y(1) - 30.0) > 1e-6) error stop "nested_vars_03 case2: y(1) mismatch"
        if (abs(y(2) - 0.0) > 1e-6) error stop "nested_vars_03 case2: y(2) mismatch"

    contains

        subroutine matvec(x, y, alpha, beta, op)
            real, intent(in) :: x(:)
            real, intent(inout) :: y(:)
            real, intent(in) :: alpha
            real, intent(in) :: beta
            character(1), intent(in) :: op
            if (abs(alpha - 1.0) > 1e-6) error stop "nested_vars_03 case2: alpha mismatch"
            if (abs(beta - 0.0) > 1e-6) error stop "nested_vars_03 case2: beta mismatch"
            if (op /= 'N') error stop "nested_vars_03 case2: op mismatch"

            y = merge(0.0, y, di_)
        end subroutine matvec

    end subroutine case_dispatch_via_non_nested_arg

end module nested_vars_03_mod

program nested_vars_03
    use nested_vars_03_mod
    implicit none

    call run_repro()
end program nested_vars_03
