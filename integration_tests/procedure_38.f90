! Test: nopass procedure pointer in derived type with assumed-shape arrays.
! Verifies pass_array_by_data handles struct member procedure pointers
! whose type declaration has array arguments.
module procedure_38_mod
    implicit none

    abstract interface
        subroutine vector_sub(x, y)
            double precision, intent(in)  :: x(:)
            double precision, intent(inout) :: y(:)
        end subroutine
    end interface

    type :: linop_type
        procedure(vector_sub), nopass, pointer :: matvec => null()
    end type

contains

    subroutine solve(A, b, x)
        class(linop_type), intent(in) :: A
        double precision, intent(in) :: b(:)
        double precision, intent(inout) :: x(:)
        call A%matvec(b, x)
    end subroutine

end module

program procedure_38
    use procedure_38_mod
    implicit none

    type(linop_type) :: op
    double precision :: b(5), x(5)

    op%matvec => my_matvec
    b = 1.0d0
    x = 0.0d0
    call solve(op, b, x)

    if (any(abs(x - 2.0d0) > 1.0d-12)) error stop

contains
    subroutine my_matvec(x, y)
        double precision, intent(in)  :: x(:)
        double precision, intent(inout) :: y(:)
        y = 2.0d0 * x
    end subroutine
end program
