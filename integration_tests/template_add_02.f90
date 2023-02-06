module template_add_m
    implicit none
    private
    public :: add_t

    requirement r(t, f)
        type :: t; end type
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
    end subroutine

end module