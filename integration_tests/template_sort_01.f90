module template_sort_01_m
    implicit none
    private
    public :: sort_t

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

    template qsort_t(T, lt, ge)
        require :: op_r(T, T, logical, lt)
        require :: op_r(T, T, logical, ge)
        private
        public :: qsort
    contains
        pure subroutine swp(lhs, rhs)
            type(T), intent(inout) :: lhs
            type(T), intent(inout) :: rhs

            type(T) :: tmp

            tmp = lhs
            lhs = rhs
            rhs = tmp
        end subroutine

        pure recursive subroutine qs(arr, low, high)
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
                        call swp(arr(last), arr(i))
                    end if
                end do
                call swp(arr(last + 1), arr(high))

                call qs(arr, low, last)
                call qs(arr, last + 2, high)
            end if
        end subroutine

        !pure recursive function qsort(arr) result(res)
        !    type(T), intent(in) :: arr(:)
        !    type(T) :: res(size(arr))
        !    type(T) :: pivot, lower(size(arr)), upper(size(arr))
        !    integer :: n, n_lower, n_upper, i
        !    n = size(arr)
        !    if (n > 1) then
        !        pivot = arr(n)
        !        n_lower = 0
        !        n_upper = 0
        !        do i=1,n-1
        !            if (le(arr(i), pivot)) then
        !                n_lower = n_lower + 1
        !                lower(n_lower) = arr(i)
        !            else if (gt(arr(i), pivot)) then
        !                n_upper = n_upper + 1
        !                upper(n_upper) = arr(i)
        !            end if
        !        end do
        !        res = [qsort(lower(1:n_lower)), pivot, qsort(upper(1:n_upper))]    ! this doesn't work in non-generic
        !    else
        !        res = arr
        !    end if
        !end function
    end template

contains
    
    ! non-generic reference
    pure recursive subroutine quicksort(arr, low, high)
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        
        integer :: i, last
        integer :: pivot
        
        if (low < high) then
            pivot = arr(high)
            last = low - 1

            do i = low, high - 1
                if (arr(i) < pivot) then
                    last = last + 1
                    call swap(arr(last), arr(i))
                end if
            end do
            call swap(arr(last + 1), arr(high))

            call quicksort(arr, low, last)
            call quicksort(arr, last + 2, high)
        end if
    end subroutine

    pure subroutine swap(lhs, rhs)
        integer, intent(inout) :: lhs
        integer, intent(inout) :: rhs

        integer :: tmp

        tmp = lhs
        lhs = rhs
        rhs = tmp

    end subroutine

    pure elemental function lt_real(lhs, rhs) result(res)
        real, intent(in) :: lhs
        real, intent(in) :: rhs
        logical :: res
        res = lhs < rhs
    end function

    pure elemental function ge_real(lhs, rhs) result(res)
        real, intent(in) :: lhs
        real, intent(in) :: rhs
        logical :: res
        res = lhs >= rhs
    end function

    pure elemental function lt_integer(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res
        res = lhs < rhs
    end function

    pure elemental function ge_integer(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res
        res = lhs >= rhs
    end function

    subroutine test_template()
        integer :: xi(10)
        real :: xr(10)
        instantiate qsort_t(integer, lt_integer, ge_integer), only: qsort_integer => qs
        instantiate qsort_t(real, lt_real, ge_real), only: qsort_real => qs
        xi = [2,4,1,5,6,24,51,3,42,2]
        xr = [2,4,1,5,6,24,51,3,42,2]
        call qsort_integer(xi, 1, 10)
        call qsort_real(xr, 1, 10)
        print *, xi
        print *, xr
    end subroutine

end module

program template_sort_01
use template_sort_01_m

call test_template()

end program