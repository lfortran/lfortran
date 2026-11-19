! Under --fast an allocatable local of a constant size becomes an array of
! the procedure's frame also when assignments reallocate it, as long as
! each gives it the constant shape it already has, which then needs no
! reallocation. An assignment of another shape keeps it allocatable.
module realloc_lhs_promote_01_m
implicit none
integer :: ticks = 0
contains
subroutine tick()
    ticks = ticks + 1
end subroutine

! Allocated by its first assignment, assigned again around a call
subroutine same_shape(a, s)
    real, intent(in) :: a(8)
    real, intent(inout) :: s
    real, allocatable :: t(:)
    t = 2*a
    s = s + t(1)
    call tick()
    t = 3*a
    s = s + t(8)
    if (size(t) /= 8 .or. lbound(t, 1) /= 1) error stop 1
end subroutine

! Assigned another shape: reallocated
subroutine other_shape(a, s)
    real, intent(in) :: a(8)
    real, intent(inout) :: s
    real, allocatable :: t(:)
    t = 2*a
    call tick()
    t = a(1:3)
    if (size(t) /= 3) error stop 2
    s = s + sum(t)
end subroutine

! Allocated with other bounds: an assignment of the same shape keeps them
subroutine other_bounds(a, s)
    real, intent(in) :: a(8)
    real, intent(inout) :: s
    real, allocatable :: t(:)
    allocate(t(0:7))
    call tick()
    t = a
    if (lbound(t, 1) /= 0 .or. t(0) /= a(1)) error stop 3
    s = s + t(7)
end subroutine

! Every frame of a recursion has its own array
recursive subroutine nested(a, depth, s)
    real, intent(in) :: a(8)
    integer, intent(in) :: depth
    real, intent(inout) :: s
    real, allocatable :: t(:)
    t = a + depth
    call tick()
    if (depth > 0) call nested(a, depth - 1, s)
    t = t + 1
    s = s + t(1)
end subroutine
end module

program realloc_lhs_promote_01
use realloc_lhs_promote_01_m
implicit none
real :: a(8), s
integer :: i
a = [(real(i), i = 1, 8)]
s = 0
call same_shape(a, s)
if (abs(s - 26) > 1e-6) error stop 10
s = 0
call other_shape(a, s)
if (abs(s - 6) > 1e-6) error stop 11
s = 0
call other_bounds(a, s)
if (abs(s - 8) > 1e-6) error stop 12
s = 0
call nested(a, 2, s)
! t(1) = 1 + depth + 1 for depth 0, 1, 2
if (abs(s - 9) > 1e-6) error stop 13
if (ticks /= 6) error stop 14
print *, "ok"
end program
