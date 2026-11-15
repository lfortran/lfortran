module realloc_lhs_hoist_05_mod
implicit none

contains

subroutine chain(a, b, c, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_0_7
    integer(4) :: __libasr_index_0_8
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: b
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: reps
    real(4), dimension(:), allocatable :: t1
    real(4), dimension(:), allocatable :: t2
    if (1 <= reps) then
        reallocate(t1(size(a, 1)))
        reallocate(t2(size(t1, 1)))
        reallocate(c(size(t2, 1)))
    end if
    do i = 1, reps
        a(1) = a(1) + real(1, kind=4)
        ! CheckBounds LHS = t1, RHS = a, b, 
        __libasr_index_0_1 = lbound(a, 1)
        __libasr_index_0_2 = lbound(b, 1)
        do __libasr_index_0_ = lbound(t1, 1), ubound(t1, 1)
            t1(__libasr_index_0_) = a(__libasr_index_0_1) + b(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
        ! CheckBounds LHS = t2, RHS = t1, a, 
        __libasr_index_0_4 = lbound(t1, 1)
        __libasr_index_0_5 = lbound(a, 1)
        do __libasr_index_0_3 = lbound(t2, 1), ubound(t2, 1)
            t2(__libasr_index_0_3) = t1(__libasr_index_0_4)*a(__libasr_index_0_5)
            __libasr_index_0_4 = __libasr_index_0_4 + 1
            __libasr_index_0_5 = __libasr_index_0_5 + 1
        end do
        ! CheckBounds LHS = c, RHS = t2, b, 
        __libasr_index_0_7 = lbound(t2, 1)
        __libasr_index_0_8 = lbound(b, 1)
        do __libasr_index_0_6 = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_6) = t2(__libasr_index_0_7) - b(__libasr_index_0_8)
            __libasr_index_0_7 = __libasr_index_0_7 + 1
            __libasr_index_0_8 = __libasr_index_0_8 + 1
        end do
    end do
end subroutine chain

subroutine chain_changing(a, c, sizes)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), dimension(3), intent(out) :: sizes
    real(4), dimension(:), allocatable :: t
    do i = 1, 3
        reallocate(t((i - 1)/1 + 1))
        __libasr_index_0_1 = 1
        do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
            t(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        reallocate(c(size(t, 1)))
        ! CheckBounds LHS = c, RHS = t, t, 
        __libasr_index_0_3 = lbound(t, 1)
        do __libasr_index_0_2 = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_2) = real(2, kind=4)*t(__libasr_index_0_3)
            __libasr_index_0_3 = __libasr_index_0_3 + 1
        end do
        sizes(i) = size(c)
    end do
end subroutine chain_changing

subroutine chain_reversed(a, t1, t2, sizes)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    real(4), dimension(:), allocatable, intent(inout) :: a
    integer(4) :: i
    integer(4), dimension(2), intent(out) :: sizes
    real(4), dimension(:), allocatable, intent(inout) :: t1
    real(4), dimension(:), allocatable, intent(inout) :: t2
    do i = 1, 2
        reallocate(t2(size(t1, 1)))
        ! CheckBounds LHS = t2, RHS = t1, t1, 
        __libasr_index_0_1 = lbound(t1, 1)
        do __libasr_index_0_ = lbound(t2, 1), ubound(t2, 1)
            t2(__libasr_index_0_) = real(2, kind=4)*t1(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        sizes(i) = size(t2)
        reallocate(t1(size(a, 1)))
        ! CheckBounds LHS = t1, RHS = a, 
        __libasr_index_0_3 = lbound(a, 1)
        do __libasr_index_0_2 = lbound(t1, 1), ubound(t1, 1)
            t1(__libasr_index_0_2) = a(__libasr_index_0_3)
            __libasr_index_0_3 = __libasr_index_0_3 + 1
        end do
    end do
end subroutine chain_reversed

end module realloc_lhs_hoist_05_mod

program realloc_lhs_hoist_05
use realloc_lhs_hoist_05_mod, only: chain
use realloc_lhs_hoist_05_mod, only: chain_changing
use realloc_lhs_hoist_05_mod, only: chain_reversed
implicit none
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
real(4), dimension(:), allocatable :: a
real(4), dimension(:), allocatable :: b
real(4), dimension(:), allocatable :: c
integer(4), dimension(2) :: sizes
integer(4), dimension(3) :: sizes3
real(4), dimension(:), allocatable :: t1
real(4), dimension(:), allocatable :: t2
__libasr_lbound_ = lbound(a, 1)
a(__libasr_lbound_ + 0) = 1.00000000e+00
a(__libasr_lbound_ + 1) = 2.00000000e+00
a(__libasr_lbound_ + 2) = 3.00000000e+00
__libasr_lbound_1 = lbound(b, 1)
b(__libasr_lbound_1 + 0) = 1.00000000e+01
b(__libasr_lbound_1 + 1) = 2.00000000e+01
b(__libasr_lbound_1 + 2) = 3.00000000e+01
call chain(a, b, c, 2)
if (.not. Allocated(c)) then
    error stop
end if
if (size(c) /= 3) then
    error stop
end if
if (Any(c /= [2.90000000e+01, 2.40000000e+01, 6.90000000e+01])) then
    error stop
end if
deallocate(c)
allocate(c(5))
call chain(a, b, c, 1)
if (size(c) /= 3) then
    error stop
end if
call chain(a, b, c, 0)
if (size(c) /= 3) then
    error stop
end if
deallocate(c)
call chain(a, b, c, 0)
if (Allocated(c)) then
    error stop
end if
allocate(t1(1), t2(7))
do __libasr_index_0_ = lbound(t1, 1), ubound(t1, 1)
    t1(__libasr_index_0_) = real(1, kind=4)
end do
call chain_reversed(a, t1, t2, sizes)
if (Any(sizes /= [1, 3])) then
    error stop
end if
if (size(t2) /= 3 .or. size(t1) /= 3) then
    error stop
end if
call chain_changing(a, c, sizes3)
if (Any(sizes3 /= [1, 2, 3])) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_05
