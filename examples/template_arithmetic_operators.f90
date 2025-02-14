module add_m
    implicit none
    private
    public :: add_t

    template arithmetic_t(T)
        private
        public :: add

        type :: T
        end type

    contains
        function add_generic(x, y)
            type(T) :: x, y, add_generic

            add_generic = x + y

        end function

        function sub_generic(x, y)
            type(T) :: x, y, sub_generic

            sub_generic = x - y

        end function

        function mul_generic(x, y)
            type(T) :: x, y, mul_generic

            mul_generic = x * y

        end function

        function div_generic(x, y)
            type(T) :: x, y, div_generic

            div_generic = x / y

        end function

        function exp_generic(x, y)
            type(T) :: x, y, exp_generic

            exp_generic = x ** y

        end function
    end template

contains

    subroutine test_template()

        ! Add two reals
        instantiate arithmetic_t(real), only: add_real => add_generic
        real :: x, y
        integer :: a, b
        x = 5.1
        y = 7.2
        print*, x, " + ", y, " = ", add_real(x, y)
        if (abs(add_real(x, y) - 12.3) > 1e-5) error stop

        ! Add two integers
        instantiate arithmetic_t(integer), only: add_integer => add_generic
        a = 5
        b = 9
        print*, a, " + ", b, " = ", add_integer(a, b)
        if (add_integer(a, b) /= 14) error stop

        ! Subtract two reals
        instantiate arithmetic_t(real), only: sub_real => sub_generic
        print*, x, " - ", y, " = ", sub_real(x, y)
        if (abs(sub_real(x, y) - (-2.1)) > 1e-5) error stop

        ! Subtract two integers
        instantiate arithmetic_t(integer), only: sub_integer => sub_generic
        print*, a, " - ", b, " = ", sub_integer(a, b)
        if (sub_integer(a, b) /= -4) error stop

        ! Multiply two reals
        instantiate arithmetic_t(real), only: mul_real => mul_generic
        print*, x, " * ", y, " = ", mul_real(x, y)
        if (abs(mul_real(x, y) - (36.72)) > 1e-5) error stop

        ! Multiply two integers
        instantiate arithmetic_t(integer), only: mul_integer => mul_generic
        print*, a, " * ", b, " = ", mul_integer(a, b)
        if (mul_integer(a, b) /= 45) error stop

        ! Divide two reals
        instantiate arithmetic_t(real), only: div_real => div_generic
        print*, x, " / ", y, " = ", div_real(x, y)
        if (abs(div_real(x, y) - (0.708333)) > 1e-5) error stop

        ! Divide two integers
        instantiate arithmetic_t(integer), only: div_integer => div_generic
        print*, a, " / ", b, " = ", div_integer(a, b)
        print*, b, " / ", a, " = ", div_integer(b, a)
        if (div_integer(a, b) /= 0) error stop
        if (div_integer(b, a) /= 1) error stop

        ! Take exponential of two reals
        instantiate arithmetic_t(real), only: exp_real => exp_generic
        print*, x, " ** ", y, " = ", exp_real(x, y)
        if (abs(exp_real(x, y) - (124309.718)) > 1e-5) error stop

        ! Take exponential of two integers
        instantiate arithmetic_t(integer), only: exp_integer => exp_generic
        print*, a, " * ", b, " = ", exp_integer(a, b)
        if (exp_integer(a, b) /= 1953125) error stop

    end subroutine

end module

program use_template_module     
use add_m      
implicit none     
   
    call test_template()
   
end program use_template_module