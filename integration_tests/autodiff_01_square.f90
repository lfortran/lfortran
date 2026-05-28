module autodiff_square_mod
    implicit none

    ! Enzyme recognises external functions whose name contains the
    ! substring "__enzyme_autodiff" and rewrites the call to compute
    ! the reverse-mode derivative of the first procedure argument.
    interface
        subroutine square__enzyme_autodiff(fn, x, dx) bind(c)
            interface
                real function fn_decal(a)
                    real, intent(in) :: a
                end function
            end interface
            procedure(fn_decal) :: fn
            real, intent(in)    :: x
            real, intent(inout) :: dx
        end subroutine
    end interface

contains

    real function square(x)
        real, intent(in) :: x
        square = x * x
    end function

end module

program test_autodiff_square
    use autodiff_square_mod
    implicit none
    real :: x, dx
    real, parameter :: tol = 1.0e-5

    x  = 3.0
    dx = 0.0

    call square__enzyme_autodiff(square, x, dx)

    print *, "d/dx [x*x] at x=3 =", dx
    if (abs(dx - 6.0) > tol) error stop "wrong derivative"
end program
