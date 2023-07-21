module template_add_01b_m
    implicit none
    private
    public :: add_t

    requirement R(T, F)
        type :: T; end type
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
            z = x + y
        end function
    end template

contains

    integer function func_arg_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    subroutine test_template()
        real :: a
        integer :: n, s

        instantiate add_t(integer, func_arg_int), only: add_integer => add_generic
        n = add_integer(5, 9)
        !s = add_integer2(5, 9, 10)
        print*, "The result is", n
    end subroutine
end module

program template_add_01b
use template_add_01b_m
implicit none

call test_template()

end program