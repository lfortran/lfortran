module template_add_m
    implicit none
    private
    public :: add_t

    requirement R(T, F)
        type, deferred :: T
        function F(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end requirement

    template add_t(T, F)
        requires R(T, F)
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains

    subroutine test_template()
        instantiate add_t(real, operator(+)), only: add_real => add_generic
        instantiate add_t(integer, operator(+)), only: add_integer => add_generic
        real :: x, y
        integer :: a, b
        x = 5.1
        y = 7.2
        print*, "The result is ", add_real(x, y)
        if (abs(add_real(x, y) - 12.3) > 1e-5) error stop

        a = 5
        b = 9
        print*, "The result is ", add_integer(a, b)
        if (add_integer(a, b) /= 14) error stop
    end subroutine
end module

program template_add
use template_add_m
implicit none

call test_template()

end program template_add
