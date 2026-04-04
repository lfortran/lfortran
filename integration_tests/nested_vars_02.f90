module nested_vars_02_mod
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

    subroutine run_repro()
        call case_struct_member_dispatch()
        call case_reassociate_pointer_then_dispatch()
    end subroutine run_repro

    subroutine case_struct_member_dispatch()
        type(linop_type) :: a
        real :: x(2), y(2)
        logical, target :: di(2)
        logical, pointer :: di_(:)

        x = [1.0, 2.0]
        y = [3.0, 4.0]
        di = [.true., .false.]

        di_ => di
        a%matvec => matvec
        call a%matvec(x, y, 1.0, 0.0, 'N')

        if (abs(y(1) - 0.0) > 1e-6) error stop "nested_vars_02 case0: y(1) mismatch"
        if (abs(y(2) - 4.0) > 1e-6) error stop "nested_vars_02 case0: y(2) mismatch"

    contains

        subroutine matvec(x, y, alpha, beta, op)
            real, intent(in) :: x(:)
            real, intent(inout) :: y(:)
            real, intent(in) :: alpha
            real, intent(in) :: beta
            character(1), intent(in) :: op
            if (abs(alpha - 1.0) > 1e-6) error stop "nested_vars_02 case0: alpha mismatch"
            if (abs(beta - 0.0) > 1e-6) error stop "nested_vars_02 case0: beta mismatch"
            if (op /= 'N') error stop "nested_vars_02 case0: op mismatch"

            y = merge(0.0, y, di_)
        end subroutine matvec

    end subroutine case_struct_member_dispatch

    subroutine case_reassociate_pointer_then_dispatch()
        type(linop_type) :: a
        real :: x(2), y(2)
        logical, target :: di_a(2), di_b(2)
        logical, pointer :: di_(:)

        x = [1.0, 2.0]
        y = [7.0, 8.0]
        di_a = [.true., .false.]
        di_b = [.false., .true.]

        di_ => di_a
        a%matvec => matvec
        call a%matvec(x, y, 1.0, 0.0, 'N')
        if (abs(y(1) - 0.0) > 1e-6) error stop "nested_vars_02 case1a: y(1) mismatch"
        if (abs(y(2) - 8.0) > 1e-6) error stop "nested_vars_02 case1a: y(2) mismatch"

        y = [7.0, 8.0]
        di_ => di_b
        call a%matvec(x, y, 1.0, 0.0, 'N')
        if (abs(y(1) - 7.0) > 1e-6) error stop "nested_vars_02 case1b: y(1) mismatch"
        if (abs(y(2) - 0.0) > 1e-6) error stop "nested_vars_02 case1b: y(2) mismatch"

    contains

        subroutine matvec(x, y, alpha, beta, op)
            real, intent(in) :: x(:)
            real, intent(inout) :: y(:)
            real, intent(in) :: alpha
            real, intent(in) :: beta
            character(1), intent(in) :: op
            if (abs(alpha - 1.0) > 1e-6) error stop "nested_vars_02: alpha mismatch"
            if (abs(beta - 0.0) > 1e-6) error stop "nested_vars_02: beta mismatch"
            if (op /= 'N') error stop "nested_vars_02: op mismatch"

            y = merge(0.0, y, di_)
        end subroutine matvec

    end subroutine case_reassociate_pointer_then_dispatch

end module nested_vars_02_mod

program nested_vars_02
    use nested_vars_02_mod
    implicit none

    call run_repro()
end program nested_vars_02