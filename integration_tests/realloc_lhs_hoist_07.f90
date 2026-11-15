! Reallocation on assignment in loops to components: a component that only
! one variable reaches stands for that variable, so its reallocation moves
! before the loop when nothing else in the loop changes the allocation of a
! part of the variable.
module realloc_lhs_hoist_07_mod
implicit none
type :: state_t
    real, allocatable :: x(:), v(:)
end type
type :: outer_t
    type(state_t) :: s
end type
contains

! q%x = p%x + 2*p%v: the component of another variable.
subroutine component(p, q, reps)
    type(state_t), intent(inout) :: p, q
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        p%x(1) = p%x(1) + 1
        q%x = p%x + 2*p%v
    end do
end subroutine

! A component of a component.
subroutine nested_component(p, o, reps)
    type(state_t), intent(inout) :: p
    type(outer_t), intent(inout) :: o
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        o%s%v = 3*p%v
    end do
end subroutine

! Two components of one variable: the reallocations stay in the loop.
subroutine two_components(p, q, reps)
    type(state_t), intent(inout) :: p, q
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        q%x = p%x
        q%v = q%x(1:i)
    end do
end subroutine

! The loop reallocates the component the shape is read from.
subroutine source_changes(p, q, sizes)
    type(state_t), intent(inout) :: p, q
    integer, intent(out) :: sizes(2)
    integer :: i
    do i = 1, 2
        q%x = p%v
        sizes(i) = size(q%x)
        deallocate(p%v)
        allocate(p%v(5))
        p%v = 1
    end do
end subroutine

! The variable has the TARGET attribute: the reallocation stays in the loop.
subroutine target_component(p, q, reps)
    type(state_t), intent(inout) :: p
    type(state_t), target, intent(inout) :: q
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        q%x = p%x
    end do
end subroutine

end module

program realloc_lhs_hoist_07
use realloc_lhs_hoist_07_mod
implicit none
type(state_t) :: p, q
type(outer_t) :: o
integer :: sizes(2)

allocate(p%x(3), p%v(3))
p%x = [1, 2, 3]
p%v = [10, 20, 30]

call component(p, q, 2)
if (.not. allocated(q%x)) error stop 1
if (size(q%x) /= 3) error stop 2
if (any(q%x /= [23.0, 42.0, 63.0])) error stop 3
deallocate(q%x)
call component(p, q, 0)
if (allocated(q%x)) error stop 4
allocate(q%x(7))
call component(p, q, 1)
if (size(q%x) /= 3) error stop 5

call nested_component(p, o, 2)
if (size(o%s%v) /= 3) error stop 6
if (any(o%s%v /= [30.0, 60.0, 90.0])) error stop 7

call two_components(p, q, 2)
if (size(q%v) /= 2) error stop 8

call source_changes(p, q, sizes)
if (any(sizes /= [3, 5])) error stop 9

deallocate(q%x)
call target_component(p, q, 2)
if (size(q%x) /= 3) error stop 10

print *, "ok"
end program
