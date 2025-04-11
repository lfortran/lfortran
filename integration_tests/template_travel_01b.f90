module template_travel_01b_m

    implicit none
    private
    public :: travel_tmpl, test_template

    requirement operation(A, B, C, op)
        type, deferred :: A
        type, deferred :: B
        type, deferred :: C

        pure function op(l, r) result(res)
            type(A), intent(in) :: l
            type(B), intent(in) :: r
            type(C) :: res
        end function
    end requirement

    template travel_tmpl(D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S)
        require :: operation(D, D, D, plus_D)
        require :: operation(T, T, T, plus_T)
        require :: operation(D, T, S, D_divided_by_T)
        require :: operation(D, S, T, D_divided_by_S)
        private
        public :: avg_S_from_T, avg_S_from_S
    contains
        pure function avg_S_from_T(d1, t1, d2, t2) result(avg)
            type(D), intent(in) :: d1, d2
            type(T), intent(in) :: t1, t2
            type(S) :: avg
            avg = D_divided_by_T(plus_D(d1, d2), plus_T(t1, t2))
        end function

        pure function avg_S_from_S(d1, s1, d2, s2) result(avg)
            type(D), intent(in) :: d1, d2
            type(S), intent(in) :: s1, s2
            type(S) :: avg
            avg = avg_S_from_T(d1, D_divided_by_S(d1, s1), d2, D_divided_by_S(d2, s2))
        end function
    end template

contains

    subroutine test_template()
        instantiate travel_tmpl(real, real, real, operator(+), operator(+), operator(/), operator(/)), &
            only: avg_real_S_from_T => avg_S_from_T, avg_real_S_from_S => avg_S_from_S
        instantiate travel_tmpl(integer, integer, integer, operator(+), operator(+), operator(/), operator(/)), &
            only: avg_integer_S_from_T => avg_S_from_T, avg_integer_S_from_S => avg_S_from_S
        real :: s1, s2
        integer :: i1, i2
        s1 = avg_real_S_from_T(1.0, 3.0, 1.5, 4.0)
        s2 = avg_real_S_from_S(1.1, s1, 2.0, s1)
        i1 = avg_integer_S_from_T(1, 3, 20, 4)
        i2 = avg_integer_S_from_S(1, i1, 15, i1)
        print *, "s1=", s1
        print *, "s2=", s2
        print *, "i1=", i1
        print *, "i2=", i2
    end subroutine

end module

program template_travel_01b
use template_travel_01b_m
implicit none

call test_template()

end program
