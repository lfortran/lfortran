module template_interface_01_m
    implicit none
    public :: test_template

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

    template sum_t(T, add, cast)
        require :: operator_r(T, T, T, add), cast_r(T, cast)
        private
        public :: generic_sum

        interface operator(+)
            procedure add
        end interface
    contains
        pure function generic_sum(arr) result(res)
            type(T), intent(in) :: arr(:)
            type(T) :: res
            integer :: n, i
            n = size(arr)
            res = cast(0)
            if (n > 0) then
                res = arr(1)
                do i=2,n
                    res = res + arr(i)
                end do
            end if       
        end function
    end template

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

    pure function simple_generic_sum {T, add, cast} (arr) result(res)
        require :: operator_r(T, T, T, add), cast_r(T, cast)
        interface operator(+)
            procedure add
        end interface
        type(T), intent(in) :: arr(:)
        type(T) :: res
        integer :: n, i
        n = size(arr)
        res = cast(0)
        if (n > 0) then
            res = arr(1)
            do i=2,n
                res = res + arr(i)
            end do
        end if    
    end function

    subroutine test_template()
        instantiate sum_t(integer, operator(+), cast_integer), only: generic_sum_integer => generic_sum
        instantiate sum_t(real, operator(+), cast_real), only: generic_sum_real => generic_sum
        integer :: ai(10), i, ri
        real :: ar(10), rr
        do i = 1, 10
            ai(i) = i
            ar(i) = i
        end do
        ri = generic_sum_integer(ai)
        rr = generic_sum_real(ar)
        print *, ri
        print *, rr
        print *, simple_generic_sum{integer, operator(+), cast_integer}(ai)
        print *, simple_generic_sum{real, operator(+), cast_real}(ar)
    end subroutine

end module

program template_interface_01
use template_interface_01_m
implicit none

call test_template()

end program