module template_simple_04_m

requirement operator_r(T, U, V, binary_func)
    type, deferred :: T
    type, deferred :: U
    type, deferred :: V
    pure elemental function binary_func(lhs, rhs) result(res)
    type(T), intent(in) :: lhs
    type(U), intent(in) :: rhs
    type(V) :: res
    end function
end requirement

requirement cast_r(T, cast)
    type, deferred :: T
    pure elemental function cast(arg) result(res)
    integer, intent(in) :: arg
    type(T) :: res
    end function
end requirement

contains

    pure function generic_sum{T, add, cast}(A) result(res)
    require :: operator_r(T, T, T, add), cast_r(T, cast)
    interface operator(+)
        procedure add
    end interface
    type(T), intent(in) :: A(:)
    type(T) :: res
    integer :: n, i
    res = cast(0)
    do i = 1, size(A)
        res = res + A(i)
    end do
    end function

    pure elemental integer function cast_integer(arg) result(r)
    integer, intent(in) :: arg
    r = arg
    end function

    pure elemental real function cast_real(arg) result(r)
    integer, intent(in) :: arg
    r = arg
    end function

    subroutine test_template()
    integer :: a_i(10), i, s_i
    real :: a_r(10), s_r
    do i = 1, size(a_i)
        a_i(i) = i
        a_r(i) = i
    end do
    s_i = generic_sum{integer, operator(+), cast_integer}(a_i)
    s_r = generic_sum{real, operator(+), cast_real}(a_r)
    print *, s_i
    print *, s_r
    if (s_i /= 55) error stop
    if (abs(s_r - 55) > 1e-5) error stop
    end subroutine

end module


program template_simple_04
use template_simple_04_m
call test_template()
end
