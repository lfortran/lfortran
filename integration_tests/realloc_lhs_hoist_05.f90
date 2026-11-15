! Reallocation on assignment in loops: chained assignments through work
! arrays, whose reallocations move before the loop one after the other.
module realloc_lhs_hoist_05_mod
implicit none
contains

! t1 = a + b; t2 = t1*a; c = t2 - b: every reallocation moves, in order.
subroutine chain(a, b, c, reps)
    real, allocatable, intent(inout) :: a(:), b(:), c(:)
    integer, intent(in) :: reps
    real, allocatable :: t1(:), t2(:)
    integer :: i
    do i = 1, reps
        a(1) = a(1) + 1
        t1 = a + b
        t2 = t1*a
        c = t2 - b
    end do
end subroutine

! The reallocation of t2 to the shape of t1 comes first: in the first
! iteration t2 takes the shape t1 has before the loop, so it cannot move.
subroutine chain_reversed(a, t1, t2, sizes)
    real, allocatable, intent(inout) :: a(:), t1(:), t2(:)
    integer, intent(out) :: sizes(2)
    integer :: i
    do i = 1, 2
        t2 = 2*t1
        sizes(i) = size(t2)
        t1 = a
    end do
end subroutine

! The first reallocation stays in the loop, as its shape changes: so does
! the second one.
subroutine chain_changing(a, c, sizes)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: sizes(3)
    real, allocatable :: t(:)
    integer :: i
    do i = 1, 3
        t = a(1:i)
        c = 2*t
        sizes(i) = size(c)
    end do
end subroutine

end module

program realloc_lhs_hoist_05
use realloc_lhs_hoist_05_mod
implicit none
real, allocatable :: a(:), b(:), c(:), t1(:), t2(:)
integer :: sizes(2), sizes3(3)

a = [1.0, 2.0, 3.0]
b = [10.0, 20.0, 30.0]

! An unallocated target.
call chain(a, b, c, 2)
if (.not. allocated(c)) error stop 1
if (size(c) /= 3) error stop 2
! a = [3, 2, 3]; t1 = [13, 22, 33]; t2 = [39, 44, 99]; c = t2 - b
if (any(c /= [29.0, 24.0, 69.0])) error stop 3
! A target of another shape is reallocated.
deallocate(c)
allocate(c(5))
call chain(a, b, c, 1)
if (size(c) /= 3) error stop 4
! A loop that does not run leaves the target alone.
call chain(a, b, c, 0)
if (size(c) /= 3) error stop 5
deallocate(c)
call chain(a, b, c, 0)
if (allocated(c)) error stop 6

! The second reallocation reads what the first one changes.
allocate(t1(1), t2(7))
t1 = 1
call chain_reversed(a, t1, t2, sizes)
if (any(sizes /= [1, 3])) error stop 7
if (size(t2) /= 3 .or. size(t1) /= 3) error stop 8

call chain_changing(a, c, sizes3)
if (any(sizes3 /= [1, 2, 3])) error stop 9

print *, "ok"
end program
