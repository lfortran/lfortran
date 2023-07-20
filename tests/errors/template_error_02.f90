module template_error_02_m
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
            type(T), intent(in) :: x, y, a
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains
    
    real function func_arg_real(x, y, a) result(z)
        real, intent(in) :: x, y, a
        z = x + y
    end function

    subroutine test_template()
        instantiate add_t(real, func_arg_real), only: add_real => add_generic
        real :: x, y
        x = 5.1
        y = 7.2
        print*, "The result is ", add_real(x, y)
        if (abs(add_real(x, y) - 12.3) > 1e-5) error stop
    end subroutine
end module

program template_error_02
use template_error_02_m
implicit none

call test_template()

end program template_error_02
