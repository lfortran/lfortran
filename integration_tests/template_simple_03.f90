module template_simple_03_m
    implicit none
    private
    public :: generic_sum

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

    pure elemental function div_integer(lhs, rhs) result(res)
        integer, intent(in) :: lhs, rhs
        integer :: res
        res = lhs / rhs
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

    pure function generic_avg {T, add, cast, div} (arr) result(res)
        require :: operator_r(T, T, T, add), cast_r(T, cast)
        require :: operator_r(T, integer, T, div)
        type(T), intent(in) :: arr(:)
        type(T) :: res
        integer :: n, i
        res = generic_sum{T, add, cast}(arr)
        !n = size(arr)
        !if (n > 0) then
        !    res = generic_sum{T, add, cast}(arr)
        !else
        !    res = cast(0)
        !end if
    end function

    subroutine test_template()
        integer :: a_i(10), i, s_i
        real :: a_r(10), s_r
        do i = 1, size(a_i)
            a_i(i) = i
            a_r(i) = i
        end do
        s_i = generic_avg{integer, operator(+), cast_integer, div_integer}(a_i)
    end subroutine

end module

program template_simple_03
    use template_simple_03_m
    implicit none

    call test_template()

end