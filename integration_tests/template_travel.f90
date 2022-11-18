module math

    implicit none
    private
    public :: add_real, slash_real

contains

    pure function add_real(x, y) result(total)
        real, intent(in) :: x, y
        real :: total
        total = x + y
    end function

    pure function slash_real(x, y) result(total)
        real, intent(in) :: x, y
        real :: total
        total = x / y
    end function

end module

module travel

    use math
    implicit none
    private 
    public :: travel_tmpl

    requirement operations(D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S)
        type :: D; end type
        type :: T; end type
        type :: S; end type

        pure function plus_D(l, r) result(total)
            type(D), intent(in) :: l, R
            type(D) :: total
        end function

        pure function plus_T(l, r) result(total)
            type(T), intent(in) :: l, R
            type(T) :: total
        end function

        pure function D_divided_by_T(n, d) result(quotient)
            type(D), intent(in) :: n
            type(T), intent(in) :: d
            type(S) :: quotient
        end function

        pure function D_divided_by_S(n, d) result(quotient)
            type(D), intent(in) :: n
            type(S), intent(in) :: d
            type(T) :: quotient
        end function
    end requirement

    template travel_tmpl(D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S)
        requires operations(D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S)
        private
        public :: avg_S_from_T
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

end module

module template_travel_m

    use math
    use travel
    implicit none

contains

    subroutine test_template()
        instantiate travel_tmpl(real, real, real, add_real, add_real, slash_real, slash_real), &
            only: avg_real_S_from_T => avg_S_from_T
        instantiate travel_tmpl(real, real, real, add_real, add_real, slash_real, slash_real), &
            only: avg_real_S_from_S => avg_S_from_S
        real :: s1, s2
        s1 = avg_real_S_from_T(1.0, 3.0, 1.5, 4.0)
        s2 = avg_real_S_from_S(1.1, 0.5, 2.0, 0.75)
        print *, "s1=", s1
        print *, "s2=", s2
    end subroutine

end module

program template_travel
use template_travel_m
implicit none

call test_template()

end program template_travel