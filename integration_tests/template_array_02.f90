module template_array_02_math

    implicit none
    private
    public :: zero_integer

contains

    pure function zero_integer() result(r)
        integer :: r
        r = 0
    end function

end module

module template_array_02_m

    use template_array_02_math
    implicit none
    private
    public :: test_template

    requirement operations{t, plus_t, zero_t}
        deffered type :: t

        pure function plus_t(l, r) result(rs)
            type(t), intent(in) :: l, r
            type(t) :: rs
        end function

        pure function zero_t() result(rs)
            type(t) :: rs
        end function
    end requirement

    template array_tmpl{t, plus_t, zero_t}
        require operations{t, plus_t, zero_t}
        private
        public :: mysum_t
    contains
        function mysum_t(n, a) result(r)
            integer, intent(in) :: n
            type(t), intent(in) :: a(n)
            type(t) :: r
            integer :: i
            r = zero_t()
            do i = 1, size(a)
                r = plus_t(r, a(i))
            end do
        end function
    end template

contains

    subroutine test_template()
        instantiate array_tmpl{integer, operator(+), zero_integer}, only: mysum_integer => mysum_t
        integer :: a(10), i, s
        do i = 1, size(a)
            a(i) = i
        end do
        s = mysum_integer(size(a), a)
        print *, s
    end subroutine

end module

program template_array_02

    use template_array_02_m
    implicit none

    call test_template()

end program
