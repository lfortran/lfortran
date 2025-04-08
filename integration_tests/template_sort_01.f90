module template_sort_01_type
    implicit none
    public :: my_type, my_type_lt

    type my_type
        real :: d
    end type

contains

    pure elemental function lt_my_type(lhs, rhs) result(res)
        type(my_type), intent(in) :: lhs, rhs
        logical :: res
        res = lhs%d < rhs%d
    end function

end module

module template_sort_01_m
    use template_sort_01_type
    implicit none
    private
    public :: sort_t, test_template

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

    template qsort_t(T, lt)
        require :: op_r(T, T, logical, lt)
        private
        public :: qsort
    contains
        subroutine swp(lhs, rhs)
            type(T), intent(inout) :: lhs
            type(T), intent(inout) :: rhs

            type(T) :: tmp

            tmp = lhs
            lhs = rhs
            rhs = tmp
        end subroutine

        recursive subroutine qs(arr, low, high)
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
    end template

contains

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
        integer :: xi(10), i
        real :: xr(10)
        type(my_type) :: xm(10)
        instantiate qsort_t(integer, lt_integer), only: qsort_integer => qs
        instantiate qsort_t(real, lt_real), only: qsort_real => qs
        instantiate qsort_t(my_type, lt_my_type), only: qsort_my_type => qs
        xi = [2,4,1,5,6,24,51,3,42,2]
        xr = [2,4,1,5,6,24,51,3,42,2]
        do i = 1, 10
            xm(i) = my_type(xr(i))
        end do
        call qsort_integer(xi, 1, 10)
        call qsort_real(xr, 1, 10)
        call qsort_my_type(xm, 1, 10)
        print *, xi
        print *, xr
        print *, xm
    end subroutine

end module

program template_sort_01
use template_sort_01_m

call test_template()

end program
