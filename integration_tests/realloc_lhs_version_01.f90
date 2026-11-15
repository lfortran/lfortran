! Reallocation on assignment in loops that the reallocation cannot leave:
! the loop runs without it when the target already has its shape, and as it
! was otherwise. The test before the loop must not read what the loop would
! not read.
module realloc_lhs_version_01_mod
implicit none
real, allocatable :: mc(:)
integer :: ticks = 0
contains

subroutine tick()
    ticks = ticks + 1
end subroutine

subroutine reset_mc()
    if (allocated(mc)) deallocate(mc)
    allocate(mc(1))
end subroutine

! A test that can leave the loop comes before the assignment.
subroutine exit_first(a, c, k)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(in) :: k
    integer :: i
    do i = 1, 3
        if (i == k) exit
        c = 2*a
    end do
end subroutine

! The assignments are conditional.
subroutine conditional(a, b, c, reps)
    real, allocatable, intent(inout) :: a(:), b(:), c(:)
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        if (mod(i, 2) == 0) then
            c = a + b
        else
            c = a - b
        end if
    end do
end subroutine

! A conditional assignment that does not run keeps the shape of the target.
subroutine never(flag, a, c)
    real, allocatable, intent(inout) :: a(:), c(:)
    logical, intent(in) :: flag
    integer :: i
    do i = 1, 3
        if (flag) c = a
    end do
end subroutine

! The shape comes from an array that may be unallocated when the
! assignment does not run.
subroutine maybe_unallocated(flag, u, c)
    real, allocatable, intent(inout) :: u(:), c(:)
    logical, intent(in) :: flag
    integer :: i
    do i = 1, 3
        if (flag) c = u
    end do
end subroutine

! The shape comes from an optional dummy argument that may be absent.
subroutine maybe_absent(c, o)
    real, allocatable, intent(inout) :: c(:)
    real, intent(in), optional :: o(:)
    integer :: i
    do i = 1, 3
        if (present(o)) c = o
    end do
end subroutine

! The target has the TARGET attribute.
subroutine target_attr(a, c, reps)
    real, allocatable, intent(inout) :: a(:)
    real, allocatable, target, intent(inout) :: c(:)
    integer, intent(in) :: reps
    real, pointer :: p(:)
    integer :: i
    do i = 1, reps
        a(1) = a(1) + 1
        c = a
        p => c
        p(2) = -1
    end do
end subroutine

! A call that cannot reach the target comes first.
subroutine call_first(a, c, reps)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        call tick()
        c = a
    end do
end subroutine

! The target is a module variable that a call in the loop reallocates.
subroutine module_target(a, sizes)
    real, allocatable, intent(inout) :: a(:)
    integer, intent(out) :: sizes(2)
    integer :: i
    do i = 1, 2
        if (i > 1) call reset_mc()
        if (i > 0) mc = a
        sizes(i) = size(mc)
        call reset_mc()
    end do
end subroutine

! The shape changes in the loop.
subroutine changing(a, c, sizes)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: sizes(3)
    integer :: i, k
    k = 1
    do i = 1, 3
        if (k > 0) c = a(1:k)
        sizes(i) = size(c)
        k = k + 1
    end do
end subroutine

! The loop deallocates the target after the assignment.
subroutine deallocates(a, c, n)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: n
    integer :: i
    n = 0
    do i = 1, 3
        if (i > 0) c = a
        n = n + size(c)
        deallocate(c)
        allocate(c(1))
    end do
end subroutine

end module

program realloc_lhs_version_01
use realloc_lhs_version_01_mod
implicit none
real, allocatable :: a(:), b(:), c(:), u(:)
integer :: sizes(3), sizes2(2), n

a = [1.0, 2.0, 3.0]
b = [10.0, 20.0, 30.0]

! The target already has the shape.
allocate(c(3))
call exit_first(a, c, 3)
if (any(c /= [2.0, 4.0, 6.0])) error stop 1
! The loop leaves before the first assignment: the target is unchanged.
deallocate(c)
call exit_first(a, c, 1)
if (allocated(c)) error stop 2
allocate(c(5))
c = 7
call exit_first(a, c, 1)
if (size(c) /= 5 .or. any(c /= 7)) error stop 3
! Another shape, and the assignment runs.
call exit_first(a, c, 2)
if (size(c) /= 3 .or. any(c /= [2.0, 4.0, 6.0])) error stop 4

call conditional(a, b, c, 3)
if (size(c) /= 3 .or. any(c /= [-9.0, -18.0, -27.0])) error stop 5
deallocate(c)
call conditional(a, b, c, 2)
if (size(c) /= 3 .or. any(c /= [11.0, 22.0, 33.0])) error stop 6

deallocate(c)
allocate(c(2))
c = 4
call never(.false., a, c)
if (size(c) /= 2 .or. any(c /= 4)) error stop 7
call never(.true., a, c)
if (size(c) /= 3) error stop 8

call maybe_unallocated(.false., u, c)
if (size(c) /= 3) error stop 9
u = [5.0, 6.0]
call maybe_unallocated(.true., u, c)
if (size(c) /= 2 .or. any(c /= [5.0, 6.0])) error stop 10

call maybe_absent(c)
if (size(c) /= 2) error stop 11
call maybe_absent(c, [1.0, 2.0, 3.0, 4.0])
if (size(c) /= 4) error stop 12

deallocate(c)
call target_attr(a, c, 2)
if (size(c) /= 3) error stop 13
if (any(c /= [3.0, -1.0, 3.0])) error stop 14
call target_attr(a, c, 1)
if (any(c /= [4.0, -1.0, 3.0])) error stop 15

ticks = 0
deallocate(c)
call call_first(a, c, 2)
if (ticks /= 2 .or. size(c) /= 3) error stop 16
call call_first(a, c, 1)
if (ticks /= 3 .or. size(c) /= 3) error stop 17

allocate(mc(3))
call module_target(a, sizes2)
if (any(sizes2 /= [3, 3])) error stop 18

deallocate(c)
allocate(c(1))
call changing(a, c, sizes)
if (any(sizes /= [1, 2, 3])) error stop 19

call deallocates(a, c, n)
if (n /= 9) error stop 20

print *, "ok"
end program
