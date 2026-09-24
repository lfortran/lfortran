! Local named constants of a templated procedure whose initializers refer to
! the template's deferred constant. Each instantiation must substitute its own
! constant into the initializers, including when the named constant is then
! used as an array bound.

module template_deferred_const_03_m
    implicit none
    private
    public :: test_three, test_five

    integer, parameter :: three = 3, five = 5

    template tmpl {n}
        deferred integer, parameter :: n
        private
        public :: get_m, get_m2, fill_sum
    contains
        function get_m() result(r)
            integer :: r
            integer, parameter :: m = n
            r = m
        end function

        function get_m2() result(r)
            integer :: r
            integer, parameter :: m2 = n*2
            r = m2
        end function

        function fill_sum() result(r)
            integer :: r
            integer, parameter :: k = n
            integer :: a(k)
            integer :: i
            do i = 1, k
                a(i) = i
            end do
            r = sum(a) + size(a)
        end function
    end template

contains

    subroutine test_three()
        instantiate tmpl {three}, only: get_m_3 => get_m, &
            get_m2_3 => get_m2, fill_sum_3 => fill_sum
        if (get_m_3() /= 3) error stop
        if (get_m2_3() /= 6) error stop
        if (fill_sum_3() /= 9) error stop
        print *, get_m_3(), get_m2_3(), fill_sum_3()
    end subroutine

    subroutine test_five()
        instantiate tmpl {five}, only: get_m_5 => get_m, &
            get_m2_5 => get_m2, fill_sum_5 => fill_sum
        if (get_m_5() /= 5) error stop
        if (get_m2_5() /= 10) error stop
        if (fill_sum_5() /= 20) error stop
        print *, get_m_5(), get_m2_5(), fill_sum_5()
    end subroutine

end module

program template_deferred_const_03
    use template_deferred_const_03_m, only: test_three, test_five
    implicit none
    call test_three()
    call test_five()
end program
