program array_bound_12
    implicit none
    type t(k)
        integer, kind :: k
        integer :: e(k:10_8)
    end type
    type(t(6)) :: x
    call s(6_8, 2)
    if (lbound(x%e, 1) /= 6 .or. ubound(x%e, 1) /= 10 .or. size(x%e) /= 5) error stop 3
contains
    subroutine s(n, m)
        integer(8), intent(in) :: n
        integer, intent(in) :: m
        integer :: c(n:10)
        integer :: d(m:n)
        if (lbound(c, 1) /= 6 .or. ubound(c, 1) /= 10 .or. size(c) /= 5) error stop 1
        if (lbound(d, 1) /= 2 .or. ubound(d, 1) /= 6 .or. size(d) /= 5) error stop 2
    end subroutine
end program
