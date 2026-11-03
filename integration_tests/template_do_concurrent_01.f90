module template_do_concurrent_01_m
    implicit none
    private
    public :: test_template

    requirement R {T, F}
        deferred type :: T
        deferred interface
            pure function F(x) result(z)
                type(T), intent(in) :: x
                real :: z
            end function
        end interface
    end requirement

    template sum_t {T, F}
        require :: R {T, F}
        private
        public :: total
    contains
        function total(a) result(r)
            type(T), intent(in) :: a(:)
            real :: r
            real :: acc
            integer :: j
            acc = 0
            do concurrent (j = 1:size(a)) reduce(+:acc)
                acc = acc + F(a(j))
            end do
            r = acc
        end function
    end template

contains

    pure function freal(x) result(z)
        real, intent(in) :: x
        real :: z
        z = x
    end function

    pure function fint(x) result(z)
        integer, intent(in) :: x
        real :: z
        z = real(x)
    end function

    subroutine test_template()
        instantiate sum_t {real, freal}, only: total_real => total
        instantiate sum_t {integer, fint}, only: total_int => total
        real :: x(4)
        integer :: k(3)
        x = [1., 2., 3., 4.]
        k = [1, 2, 3]
        if (abs(total_real(x) - 10) > 1e-6) error stop 1
        if (abs(total_int(k) - 6) > 1e-6) error stop 2
        print *, "ok"
    end subroutine
end module

program template_do_concurrent_01
use template_do_concurrent_01_m
call test_template()
end program template_do_concurrent_01
