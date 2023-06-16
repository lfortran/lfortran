module template_add_m
    implicit none
    private
    public :: add_t

    requirement R(T)
        type :: T; end type
    end requirement

    template add_t(T)
        requires R(T)
        private
        public :: add_generic
    contains
        function add_generic(x, y, z) result(s)
            type(T), intent(in) :: x, y, z
            type(T) :: s
            print*, "x, y, z, x+y+z =", x, y, z, x+y+z
            s = x + y + z
            print *, s
        end function
    end template

contains

    function add_integer2(x, y, z) result(s)
        integer, intent(in) :: x, y, z
        integer :: s
        print*, "x, y, z, x+y+z =", x, y, z, x+y+z
        s = x + y + z
        print *, s
    end function

    subroutine test_template()
        real :: a
        integer :: n, s

        instantiate add_t(real), only: add_real => add_generic
        a = add_real(5.1, 7.2, 10.0)
        print*, "The result is ", a

        instantiate add_t(integer), only: add_integer => add_generic
        n = add_integer(5, 9, 10)
        s = add_integer2(5, 9, 10)
        print*, "The result is ", n, s
    end subroutine
end module

program template_add
use template_add_m
implicit none

call test_template()

end program template_add