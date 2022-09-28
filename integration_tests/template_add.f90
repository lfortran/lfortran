module add_m
    implicit none
    private
    public :: add_t

    template add_t(T)
        private
        public :: add

        type :: T
        end type

    contains
        function add_generic(x, y)
            type(T) :: x, y, add_generic

            add_generic = x + y

        end function
    end template

contains

    subroutine test_template()
        instantiate add_t(real), only: add_real => add_generic
        real :: x, y
        integer :: a, b
        x = 5.1
        y = 7.2
        print*, "The result is ", add_real(x, y)
        if (abs(add_real(x, y) - 12.3) > 1e-5) error stop

        instantiate add_t(integer), only: add_integer => add_generic
        a = 5
        b = 9
        print*, "The result is ", add_integer(a, b)
        if (add_integer(a, b) /= 14) error stop

    end subroutine

end module

program use_template_module
use add_m
implicit none

    call test_template()

end program use_template_module