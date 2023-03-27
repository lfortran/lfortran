module template_add_03_m
    implicit none
    private
    public :: add_t

    requirement r(t, f)
        type, deferred :: t
        function f(x, y) result(z)
            type(t), intent(in) :: x, y, a
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

    subroutine test_template()
        instantiate add_t(real, operator(+)), only: add_real => add_generic
        real :: x, y
        x = 5.1
        y = 7.2
        print*, "The result is ", add_real(x, y)
        if (abs(add_real(x, y) - 12.3) > 1e-5) error stop
    end subroutine

end module

program template_add_03
use template_add_03_m
implicit none

call test_template()

end program
