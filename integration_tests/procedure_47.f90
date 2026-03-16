module bspline_sub_module_47
    implicit none
    private
    abstract interface
        function b1fqad_func(x) result(f)
        implicit none
        real(4),intent(in) :: x
        real(4)            :: f
        end function b1fqad_func
    end interface
    public :: b1fqad_func, dbsgq8
contains
    subroutine dbsgq8(fun, res)
        implicit none
        procedure(b1fqad_func)  :: fun
        real(4), intent(out)    :: res
        call g8(res)
    contains
        subroutine g8(res)
            real(4),intent(out)    :: res
            real(4),dimension(8)   :: v
            v(2) = 3.0
            res = fun(v(2))
        end subroutine g8
    end subroutine dbsgq8
end module bspline_sub_module_47

program procedure_47
    use bspline_sub_module_47
    implicit none
    real(4) :: res

    call dbsgq8(square, res)
    if (abs(res - 9.0) > 1e-6) error stop

    call dbsgq8(cube, res)
    if (abs(res - 27.0) > 1e-6) error stop

contains
    function square(x) result(f)
        implicit none
        real(4),intent(in) :: x
        real(4)            :: f
        f = x * x
    end function square

    function cube(x) result(f)
        implicit none
        real(4),intent(in) :: x
        real(4)            :: f
        f = x * x * x
    end function cube
end program procedure_47