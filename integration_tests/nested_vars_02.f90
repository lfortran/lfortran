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

        if (abs(y(1) - 0.0) > 1e-6) error stop "nested_vars_02: y(1) mismatch"
        if (abs(y(2) - 4.0) > 1e-6) error stop "nested_vars_02: y(2) mismatch"

    contains

        subroutine matvec(x, y, alpha, beta, op)
            real, intent(in) :: x(:)
            real, intent(inout) :: y(:)
            real, intent(in) :: alpha
            real, intent(in) :: beta
            character(1), intent(in) :: op

            y = merge(0.0, y, di_)
        end subroutine matvec

    end subroutine run_repro

end module nested_vars_02_mod

program nested_vars_02
    use nested_vars_02_mod
    implicit none

    call run_repro()
end program nested_vars_02
