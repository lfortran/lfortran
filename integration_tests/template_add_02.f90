module template_add_02_m
    implicit none
    private
    public :: add_t

    requirement r(t, f)
        type, deferred :: t
        function f(x, y) result(z)
            type(t), intent(in) :: x, y
            type(t) :: z
        end function
    end requirement

    template add_t(t, f)
        requires r(t, f)
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(t), intent(in) :: x, y
            type(t) :: z
            z = f(x, y)
        end function
    end template

    interface operator (+)
        module procedure func_arg_real
    end interface

contains

    real function func_arg_real(x, y) result(z)
        real, intent(in) :: x, y
        z = x + y
    end function

    integer function func_arg_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    subroutine test_template()
        instantiate add_t(real, operator(+)), only: add_real => add_generic
        real :: x, y
        x = 5.1
        y = 7.2
        print*, "The result is ", add_real(x, y)
        if (abs(add_real(x, y) - 12.3) > 1e-5) error stop
    end subroutine

end module

program template_add_02
use template_add_02_m
implicit none

call test_template()

end program
