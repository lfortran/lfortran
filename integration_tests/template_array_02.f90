module template_array_02_math

    implicit none
    private
    public :: add_integer, zero_integer, add_real, zero_real

contains

    pure function add_integer(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x + y
    end function

    pure function zero_integer(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = 0
    end function

    pure function add_real(x, y) result(r)
        real, intent(in) :: x, y
        real :: r
        r = x + y
    end function

    pure function zero_real(x) result(r)
        real, intent(in) :: x
        real :: r
        r = 0
    end function

end module

module template_array_02_m

    use template_array_02_math
    implicit none
    private
    public :: test_template

    requirement operations(t, plus_t, zero_t)
        type, deferred :: t

        pure function plus_t(l, r) result(rs)
            type(t), intent(in) :: l, r
            type(t) :: rs
        end function

        pure function zero_t(l) result(rs)
            type(t), intent(in) :: l
            type(t) :: rs
        end function
    end requirement

    template array_tmpl(t, plus_t, zero_t)
        require :: operations(t, plus_t, zero_t)
        private
        public :: mysum_t
    contains
        function mysum_t(a) result(r)
            type(t), intent(in) :: a(:)
            type(t) :: r
            integer :: i
            r = zero_t(a(1))
            do i = 1, size(a)
                r = plus_t(r, a(i))
            end do
        end function

        function mysum_t_n(n, a) result(r)
            integer, intent(in) :: n
            type(t), intent(in) :: a(n)
            type(t) :: r
            integer :: i
            r = zero_t(a(1))
            do i = 1, size(a)
                r = plus_t(r, a(i))
            end do
        end function
    end template

contains

    subroutine test_template()
        instantiate array_tmpl(integer, add_integer, zero_integer), only: &
            mysum_integer => mysum_t, mysum_integer_n => mysum_t_n
        integer :: a(10), b(10), i, sa, sb
        do i = 1, size(a)
            a(i) = i
            b(i) = i
        end do
        sa = mysum_integer(a)
        sb = mysum_integer_n(size(b), b)
        print *, sa
        print *, sb
    end subroutine

end module

program template_array_02

    use template_array_02_math
    use template_array_02_m
    implicit none

    call test_template()

end
