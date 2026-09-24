! A real(16) local named constant of a templated procedure whose initializer
! is a product of the template's deferred constant. Each instantiation must
! give the constants the values of its own constant.

module template_deferred_const_05_m
    implicit none
    private
    public :: test_three, test_five

    integer, parameter :: three = 3, five = 5

    template tmpl {n}
        deferred integer, parameter :: n
        private
        public :: real16
    contains
        function real16() result(r)
            real(8) :: r
            real(16), parameter :: x = n*0.1_16, y = x*2 + 1
            r = real(y, 8)
        end function
    end template

contains

    subroutine test_three()
        instantiate tmpl {three}, only: real16_3 => real16
        if (abs(real16_3() - 1.6d0) > 1d-12) error stop
        print *, real16_3()
    end subroutine

    subroutine test_five()
        instantiate tmpl {five}, only: real16_5 => real16
        if (abs(real16_5() - 2.0d0) > 1d-12) error stop
        print *, real16_5()
    end subroutine

end module

program template_deferred_const_05
    use template_deferred_const_05_m, only: test_three, test_five
    implicit none
    call test_three()
    call test_five()
end program
