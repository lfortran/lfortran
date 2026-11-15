module realloc_lhs_hoist_09_mod
implicit none
type :: t
    real(4), dimension(:), allocatable :: x
end type t

contains

subroutine call_changes(q, a, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    integer(4) :: i
    integer(4), intent(out) :: n
    type(t), intent(inout) :: q
    n = 0
    do i = 1, 3
        reallocate(q%x(size(a, 1)))
        ! CheckBounds LHS = q%x, RHS = a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
            q%x(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        n = n + size(q%x)
        call shrink(q)
    end do
end subroutine call_changes

subroutine call_reads(q, a, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    integer(4) :: i
    integer(4), intent(out) :: n
    type(t), intent(inout) :: q
    n = 0
    reallocate(q%x(size(a, 1)))
    do i = 1, 3
        ! CheckBounds LHS = q%x, RHS = a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
            q%x(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        call peek(q, n)
    end do
end subroutine call_reads

subroutine element_assignment(p, r, a, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    integer(4) :: i
    integer(4), intent(out) :: n
    type(t), dimension(:), intent(inout) :: p
    type(t), intent(in) :: r
    n = 0
    do i = 1, 3
        reallocate(p(1)%x(size(a, 1)))
        ! CheckBounds LHS = p(1)%x, RHS = a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(p(1)%x, 1), ubound(p(1)%x, 1)
            p(1)%x(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        n = n + size(p(1)%x)
        p(1) = r
    end do
end subroutine element_assignment

subroutine peek(s, n)
    integer(4), intent(inout) :: n
    type(t), intent(in) :: s
    n = n + size(s%x)
end subroutine peek

subroutine shrink(s)
    integer(4) :: __libasr_index_0_
    type(t), intent(inout) :: s
    deallocate(s%x)
    allocate(s%x(1))
    do __libasr_index_0_ = lbound(s%x, 1), ubound(s%x, 1)
        s%x(__libasr_index_0_) = real(0, kind=4)
    end do
end subroutine shrink

end module realloc_lhs_hoist_09_mod

program realloc_lhs_hoist_09
use realloc_lhs_hoist_09_mod, only: call_changes
use realloc_lhs_hoist_09_mod, only: call_reads
use realloc_lhs_hoist_09_mod, only: element_assignment
use realloc_lhs_hoist_09_mod, only: peek
use realloc_lhs_hoist_09_mod, only: shrink
use realloc_lhs_hoist_09_mod, only: t
implicit none
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
real(4), dimension(:), allocatable :: a
integer(4) :: n
type(t), dimension(2) :: p
type(t) :: q
type(t) :: r
__libasr_lbound_ = lbound(a, 1)
a(__libasr_lbound_ + 0) = 1.00000000e+00
a(__libasr_lbound_ + 1) = 2.00000000e+00
a(__libasr_lbound_ + 2) = 3.00000000e+00
call call_changes(q, a, n)
if (n /= 9) then
    error stop
end if
if (size(q%x) /= 1) then
    error stop
end if
call call_reads(q, a, n)
if (n /= 9) then
    error stop
end if
__libasr_lbound_1 = lbound(r%x, 1)
r%x(__libasr_lbound_1 + 0) = 5.00000000e+00
call element_assignment(p, r, a, n)
if (n /= 9) then
    error stop
end if
if (size(p(1)%x) /= 1) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_09
