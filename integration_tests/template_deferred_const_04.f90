! Local named constants of a templated procedure whose initializers are
! expressions of the template's deferred constant, used as array bounds. Each
! instantiation must give the arrays the extents of its own constant.

module template_deferred_const_04_m
    implicit none
    private
    public :: test_three, test_five

    integer, parameter :: three = 3, five = 5

    template tmpl {n}
        deferred integer, parameter :: n
        private
        public :: size_mul, size_add, size_chain, fill_sum
    contains
        function size_mul() result(r)
            integer :: r
            integer, parameter :: m = n*2
            integer :: y(m)
            r = size(y)
        end function

        function size_add() result(r)
            integer :: r
            integer, parameter :: m = n+1
            integer :: y(m)
            r = size(y)
        end function

        function size_chain() result(r)
            integer :: r
            integer, parameter :: a = n+1, b = a*2
            integer :: y(b)
            r = size(y)
        end function

        function fill_sum() result(r)
            integer :: r
            integer, parameter :: m = n*2
            integer :: y(m)
            integer :: i
            do i = 1, m
                y(i) = i
            end do
            r = sum(y)
        end function
    end template

contains

    subroutine test_three()
        instantiate tmpl {three}, only: size_mul_3 => size_mul, &
            size_add_3 => size_add, size_chain_3 => size_chain, &
            fill_sum_3 => fill_sum
        if (size_mul_3() /= 6) error stop
        if (size_add_3() /= 4) error stop
        if (size_chain_3() /= 8) error stop
        if (fill_sum_3() /= 21) error stop
        print *, size_mul_3(), size_add_3(), size_chain_3(), fill_sum_3()
    end subroutine

    subroutine test_five()
        instantiate tmpl {five}, only: size_mul_5 => size_mul, &
            size_add_5 => size_add, size_chain_5 => size_chain, &
            fill_sum_5 => fill_sum
        if (size_mul_5() /= 10) error stop
        if (size_add_5() /= 6) error stop
        if (size_chain_5() /= 12) error stop
        if (fill_sum_5() /= 55) error stop
        print *, size_mul_5(), size_add_5(), size_chain_5(), fill_sum_5()
    end subroutine

end module

program template_deferred_const_04
    use template_deferred_const_04_m, only: test_three, test_five
    implicit none
    call test_three()
    call test_five()
end program
