module template_sort_02_m
    implicit none

    requirement op_r(T, U, V, op_func)
        type, deferred :: T
        type, deferred :: U
        type, deferred :: V
        pure elemental function op_func(lhs, rhs) result(res)
            type(T), intent(in) :: lhs
            type(T), intent(in) :: rhs
            type(V) :: res
        end function
    end requirement

contains
    
    subroutine swap {T} (lhs, rhs)
        type, deferred :: T
        type(T), intent(inout) :: lhs
        type(T), intent(inout) :: rhs

        type(T) :: tmp

        tmp = lhs
        lhs = rhs
        rhs = tmp
    end subroutine

    ! non-generic reference
    recursive subroutine quicksort {T, lt} (arr, low, high)
        require :: op_r(T, T, logical, lt)
        type(T), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        
        integer :: i, last
        type(T) :: pivot
        
        if (low < high) then
            pivot = arr(high)
            last = low - 1

            do i = low, high - 1
                if (lt(arr(i), pivot)) then
                    last = last + 1
                    call swap{T}(arr(last), arr(i))
                end if
            end do
            call swap{T}(arr(last + 1), arr(high))

            call quicksort(arr, low, last)
            call quicksort(arr, last + 2, high)
        end if
    end subroutine

    pure elemental function lt_real(lhs, rhs) result(res)
        real, intent(in) :: lhs
        real, intent(in) :: rhs
        logical :: res
        res = lhs < rhs
    end function

    pure elemental function lt_integer(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res
        res = lhs < rhs
    end function

    subroutine test_template()
        integer :: xi(10)
        real :: xr(10)
        xi = [2,4,1,5,6,24,51,3,42,2]
        xr = [2,4,1,5,6,24,51,3,42,2]
        call quicksort{integer, lt_integer}(xi, 1, 10)
        call quicksort{real, lt_real}(xr, 1, 10)
        print *, xi
        print *, xr
    end subroutine

end module

program template_sort_02
use template_sort_02_m
implicit none

call test_template()

end program