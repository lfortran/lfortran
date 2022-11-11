module sum_m
    private
    public :: sum_t

    requirement R(T, Tadd, Tzero)
        type :: T; end type
        function Tadd(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
        function Tzero() result(z)
            type(T) :: z
        end function
    end requirement

    template sum_t(T, Tadd, Tzero)
        requires R(T, Tadd, Tzero)
        private
        public :: sum_generic
    contains

        function sum_generic(x) result(r)
        type(T), intent(in) :: x(:)
        type(T) :: r
        integer :: i
        r = Tzero()
        do i = 1, size(x)
            r = Tadd(r, x(i))
        end do
        end function

    end template

contains

    real function real_add(x, y) result(z)
        real, intent(in) :: x, y
        z = x + y
    end function

    real function real_zero() result(z)
        z = 0
    end function

    integer function int_add(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    integer function int_zero() result(z)
        z = 0
    end function

    subroutine test_template()
    instantiate sum_t(real, real_add, real_zero), only: sum_real => sum_generic
    instantiate sum_t(integer, int_add, int_zero), only: sum_integer => sum_generic
    real :: x(10)
    integer :: y(10)
    x = 1
    print*, "The result is ", sum_real(x)
    if (abs(sum_real(x) - 10.0) > 1e-5) error stop

    y = 1
    print*, "The result is ", sum_integer(a, b)
    if (sum_integer(a, b) /= 10) error stop
    end subroutine

end module



program sum
use sum_m, only: test_template
call test_template()
end program
