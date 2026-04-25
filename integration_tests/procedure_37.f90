module procedure_37_mod
    implicit none

    abstract interface
        subroutine vector_sub(x, y, alpha, beta)
            real, intent(in) :: x(:)
            real, intent(inout) :: y(:)
            real, intent(in) :: alpha
            real, intent(in) :: beta
        end subroutine
    end interface

    type :: linop_type
        procedure(vector_sub), nopass, pointer :: matvec => null()
    end type

contains

    subroutine my_matvec(x, y, alpha, beta)
        real, intent(in) :: x(:)
        real, intent(inout) :: y(:)
        real, intent(in) :: alpha
        real, intent(in) :: beta
        integer :: i
        do i = 1, size(x)
            y(i) = alpha * x(i) + beta * y(i)
        end do
    end subroutine

end module procedure_37_mod

program procedure_37
    use procedure_37_mod
    implicit none
    type(linop_type) :: A
    real :: x(3), y(3)

    A%matvec => my_matvec
    x = [1.0, 2.0, 3.0]
    y = [10.0, 20.0, 30.0]

    call A%matvec(x, y, alpha=2.0, beta=1.0)

    if (abs(y(1) - 12.0) > 1e-6) error stop
    if (abs(y(2) - 24.0) > 1e-6) error stop
    if (abs(y(3) - 36.0) > 1e-6) error stop

    print *, "All tests passed."
end program procedure_37
