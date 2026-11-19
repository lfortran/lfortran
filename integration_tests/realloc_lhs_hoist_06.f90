! Reallocation on assignment in DO WHILE loops: the reallocation moves
! before the loop, under its condition, when the condition calls no
! procedure and does not read the target, and nothing can leave the loop
! before it.
module realloc_lhs_hoist_06_mod
implicit none
integer :: calls = 0
contains

logical function counted_below(i, n)
    integer, intent(in) :: i, n
    calls = calls + 1
    counted_below = i < n
end function

subroutine while_add(a, b, c, reps)
    real, allocatable, intent(inout) :: a(:), b(:), c(:)
    integer, intent(in) :: reps
    integer :: i
    i = 0
    do while (i < reps)
        i = i + 1
        a(1) = a(1) + 1
        c = a + b
    end do
end subroutine

! The condition reads the target: the reallocation stays in the loop.
subroutine while_reads_target(a, c, trips)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: trips
    trips = 0
    do while (size(c) < 3)
        trips = trips + 1
        c = a(1:size(c) + 1)
    end do
end subroutine

! The condition calls a function: it is not evaluated an extra time.
subroutine while_calls(a, c, reps)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(in) :: reps
    integer :: i
    i = 0
    do while (counted_below(i, reps))
        i = i + 1
        c = a
    end do
end subroutine

! The loop can leave before the assignment.
subroutine while_exit_first(a, c, k)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(in) :: k
    integer :: i
    i = 0
    do while (i < 3)
        i = i + 1
        if (i == k) exit
        c = a
    end do
end subroutine

end module

program realloc_lhs_hoist_06
use realloc_lhs_hoist_06_mod
implicit none
real, allocatable :: a(:), b(:), c(:)
integer :: trips

a = [1.0, 2.0, 3.0]
b = [10.0, 20.0, 30.0]

! An unallocated target.
call while_add(a, b, c, 3)
if (.not. allocated(c)) error stop 1
if (size(c) /= 3) error stop 2
if (any(c /= [14.0, 22.0, 33.0])) error stop 3
! A loop that does not run leaves the target alone.
deallocate(c)
allocate(c(2))
c = 5
call while_add(a, b, c, 0)
if (size(c) /= 2) error stop 4
if (any(c /= 5)) error stop 5
! A target of another shape is reallocated.
call while_add(a, b, c, 1)
if (size(c) /= 3) error stop 6

deallocate(c)
allocate(c(1))
call while_reads_target(a, c, trips)
if (trips /= 2 .or. size(c) /= 3) error stop 7

calls = 0
deallocate(c)
call while_calls(a, c, 2)
if (calls /= 3) error stop 8
if (size(c) /= 3) error stop 9
calls = 0
deallocate(c)
call while_calls(a, c, 0)
if (calls /= 1) error stop 10
if (allocated(c)) error stop 11

call while_exit_first(a, c, 1)
if (allocated(c)) error stop 12
call while_exit_first(a, c, 2)
if (size(c) /= 3) error stop 13

print *, "ok"
end program
