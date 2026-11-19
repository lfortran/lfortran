! Reallocation on assignment to a component in a loop that also changes the
! allocation of the component in other ways: a call that takes the variable
! as a derived-type dummy argument, which can reallocate its allocatable
! components, and the assignment of a whole element of an array of a derived
! type. The reallocation must stay in the loop.
module realloc_lhs_hoist_09_mod
implicit none
type :: t
    real, allocatable :: x(:)
end type
contains

subroutine shrink(s)
    type(t), intent(inout) :: s
    deallocate(s%x)
    allocate(s%x(1))
    s%x = 0
end subroutine

subroutine peek(s, n)
    type(t), intent(in) :: s
    integer, intent(inout) :: n
    n = n + size(s%x)
end subroutine

! A call changes the allocation of q%x through a nonallocatable dummy.
subroutine call_changes(q, a, n)
    type(t), intent(inout) :: q
    real, allocatable, intent(inout) :: a(:)
    integer, intent(out) :: n
    integer :: i
    n = 0
    do i = 1, 3
        q%x = a
        n = n + size(q%x)
        call shrink(q)
    end do
end subroutine

! A call that takes q as INTENT(IN) cannot change it.
subroutine call_reads(q, a, n)
    type(t), intent(inout) :: q
    real, allocatable, intent(inout) :: a(:)
    integer, intent(out) :: n
    integer :: i
    n = 0
    do i = 1, 3
        q%x = a
        call peek(q, n)
    end do
end subroutine

! The assignment of an element reallocates the components of the element.
subroutine element_assignment(p, r, a, n)
    type(t), intent(inout) :: p(:)
    type(t), intent(in) :: r
    real, allocatable, intent(inout) :: a(:)
    integer, intent(out) :: n
    integer :: i
    n = 0
    do i = 1, 3
        p(1)%x = a
        n = n + size(p(1)%x)
        p(1) = r
    end do
end subroutine

end module

program realloc_lhs_hoist_09
use realloc_lhs_hoist_09_mod
implicit none
type(t) :: q, r
type(t) :: p(2)
real, allocatable :: a(:)
integer :: n

a = [1.0, 2.0, 3.0]
call call_changes(q, a, n)
if (n /= 9) error stop 1
if (size(q%x) /= 1) error stop 2

call call_reads(q, a, n)
if (n /= 9) error stop 3

r%x = [5.0]
call element_assignment(p, r, a, n)
if (n /= 9) error stop 4
if (size(p(1)%x) /= 1) error stop 5

print *, "ok"
end program
