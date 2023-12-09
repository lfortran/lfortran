module template_simple_04_m
    implicit none

    requirement operator_r(T, func)
        type, deferred :: T
        pure elemental function func(lhs, rhs) result(res)
            type(T), intent(in) :: lhs, rhs
            type(T) :: res
        end function
    end requirement

contains

    pure function g {T} (a) result(r)
        type, deferred :: T
        type(T), intent(in) :: a
        type(T) :: r
        r = a
    end function

    pure function f {T, add} (a) result(r)
        require :: operator_r(T, add)
        type(T), intent(in) :: a
        type(T) :: r
        r = add(g{T}(a), g{T}(a))
    end function
!
    pure function h {T} (a) result(r)
        type, deferred :: T
        type(T), intent(in) :: a
        type(T) :: r, b
        b = g{T}(a)
        if (.false.) then
            r = h(a)
        end if
    end function

    subroutine test_template()
        integer :: a, r
        a = 10
        r = g {integer} (a)
        print *, r
        r = f {integer, operator(+)} (a)
        print *, r
        r = h {integer} (a)
        !print *, r
    end subroutine

end module

program template_simple_04
use template_simple_04_m
implicit none

call test_template()

end program