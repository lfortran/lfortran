module realloc_lhs_hoist_14_mod
implicit none
real(4), dimension(:), allocatable :: g

contains

real(4) function shrink()
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    shrink = g(size(g))
    reallocate(g((size(g) - 1 - 1)/1 + 1))
    __libasr_index_0_1 = 1
    do __libasr_index_0_ = lbound(g, 1), ubound(g, 1)
        g(__libasr_index_0_) = g(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end function shrink

subroutine shrunk(a, n, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: a
    integer(4) :: i
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    s = real(0, kind=4)
    do i = 1, n
        reallocate(g(size(a, 1)))
        ! CheckBounds LHS = g, RHS = a, a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(g, 1), ubound(g, 1)
            g(__libasr_index_0_) = a(__libasr_index_0_1) + real(1, kind=4)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        s = s + shrink()
    end do
end subroutine shrunk

integer(4) function wrap(i, n)
    integer(4), intent(in) :: i
    integer(4), intent(in) :: n
    wrap = 1 + i - n*i/n
end function wrap

subroutine wrapped(a, n, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: a
    integer(4) :: i
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    s = real(0, kind=4)
    if (1 <= n) then
        reallocate(g(size(a, 1)))
    end if
    do i = 1, n
        ! CheckBounds LHS = g, RHS = a, a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(g, 1), ubound(g, 1)
            g(__libasr_index_0_) = a(__libasr_index_0_1) + real(1, kind=4)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        s = s + g(wrap(i, size(g)))
    end do
end subroutine wrapped

end module realloc_lhs_hoist_14_mod

program realloc_lhs_hoist_14
use realloc_lhs_hoist_14_mod, only: g
use realloc_lhs_hoist_14_mod, only: shrink
use realloc_lhs_hoist_14_mod, only: shrunk
use realloc_lhs_hoist_14_mod, only: wrap
use realloc_lhs_hoist_14_mod, only: wrapped
implicit none
real(4), dimension(4) :: a
real(4) :: s
a(1) = 1.00000000e+00
a(2) = 2.00000000e+00
a(3) = 3.00000000e+00
a(4) = 4.00000000e+00
call wrapped(a, 5, s)
if (Abs(s - real(17, kind=4)) > 9.99999997e-07) then
    error stop
end if
call shrunk(a, 3, s)
if (Abs(s - real(15, kind=4)) > 9.99999997e-07) then
    error stop
end if
if (size(g) /= 3) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_14
