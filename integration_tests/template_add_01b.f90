module template_add_01b_m
    implicit none
    private
    public :: add_t, test_template

    requirement R{T, F}
        deferred type :: T
        interface operator (+)
            procedure F
        end interface
        function F(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end requirement

    template add_t{T, F}
        require R{T, F}
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            !print*, "x, y, z, x+y+z =", x, y, z, x+y+z
            z = x + y
        end function
        function add_generic2(x, y, z) result(s)
            type(T), intent(in) :: x, y, z
            type(T) :: s
            s = x + y + z
        end function
    end template

contains

    subroutine test_template()
        integer :: n1, n2

        instantiate add_t{integer, operator(+)}, only: add_integer => add_generic, add_integer2 => add_generic2
        n1 = add_integer(5, 9)
        n2 = add_integer2(5, 9, 10)
        print*, "The result is", n1
        print*, "The result is", n2
    end subroutine
end module

program template_add_01b
use template_add_01b_m
implicit none

call test_template()

end program