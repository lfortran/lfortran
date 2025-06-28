module template_simple_01_m
    implicit none
    private
    public :: generic_sum, test_template

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

    pure elemental function cast_integer(arg) result(res)
        integer, intent(in) :: arg
        integer :: res
        res = 0
    end function

    pure elemental function cast_real(arg) result(res)
        integer, intent(in) :: arg
        real :: res
        res = 0.0
    end function

    pure function generic_sum {T, add, cast} (arr) result(res)
        require :: operator_r(T, T, T, add), cast_r(T, cast)
        type(T), intent(in) :: arr(:)
        type(T) :: res
        integer :: n, i
        n = size(arr)
        res = cast(0)
        if (n > 0) then
            res = arr(1)
            do i=2,n
                res = add(res, arr(i))
            end do
        end if
    end function

    subroutine test_template()
        instantiate generic_sum(integer, operator(+), cast_integer), only: generic_sum_integer => generic_sum
        instantiate generic_sum(real, operator(+), cast_real), only: generic_sum_real => generic_sum
        integer :: a_i(10), i, s_i
        real :: a_r(10), s_r
        do i = 1, size(a_i)
            a_i(i) = i
            a_r(i) = i
        end do
        s_i = generic_sum_integer(a_i)
        s_r = generic_sum_real(a_r)
        print *, s_i
        print *, s_r
    end subroutine

end module

program template_simple_01
    use template_simple_01_m
    implicit none

    call test_template()

end