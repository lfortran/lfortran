! Reallocation on assignment inside DO loops: the reallocation of an array
! whose shape the loop does not change may be done once, before the loop,
! only when that cannot be observed.
module realloc_lhs_hoist_01_mod
implicit none
real, allocatable :: mc(:)
contains

subroutine reset_mc()
    if (allocated(mc)) deallocate(mc)
    allocate(mc(1))
    mc = -1
end subroutine

subroutine shrink(x)
    real, allocatable, intent(inout) :: x(:)
    deallocate(x)
    allocate(x(1))
    x = 0
end subroutine

subroutine scale(x)
    real, intent(inout) :: x(:)
    x = 2 * x
end subroutine

subroutine grow(x)
    real, allocatable, intent(inout) :: x(:)
    real, allocatable :: y(:)
    allocate(y(size(x) + 1))
    y(1:size(x)) = x
    y(size(y)) = size(y)
    call move_alloc(y, x)
end subroutine

! The case the transformation is for: three distinct allocatable dummies.
subroutine add(a, b, c, reps)
    real, allocatable, intent(inout) :: a(:), b(:), c(:)
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        a(1) = a(1) + 1
        c = a + b
    end do
end subroutine

! A loop that runs as many times as the target has elements: the bounds are
! evaluated before the first reallocation.
subroutine bounds_name_target(a, c, trips)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: trips
    integer :: i
    trips = 0
    do i = 1, size(c)
        trips = trips + 1
        c = a
    end do
end subroutine

subroutine exit_first(a, c, k)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(in) :: k
    integer :: i
    do i = 1, 3
        if (i == k) exit
        c = a
    end do
end subroutine

subroutine cycle_all(a, c)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer :: i
    do i = 1, 3
        if (i <= 3) cycle
        c = a
    end do
end subroutine

subroutine return_first(a, c)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer :: i
    do i = 1, 3
        if (i == 1) return
        c = a
    end do
end subroutine

subroutine conditional(a, c, flag)
    real, allocatable, intent(inout) :: a(:), c(:)
    logical, intent(in) :: flag
    integer :: i
    do i = 1, 3
        if (flag) c = a
    end do
end subroutine

! The body reads the target before the assignment.
subroutine observe_before(a, c, seen)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: seen
    integer :: i
    seen = 0
    do i = 1, 2
        if (allocated(c)) seen = seen + size(c)
        c = a
    end do
end subroutine

! An EXIT of an inner loop does not leave the iteration.
subroutine inner_exit(a, c, found)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: found
    integer :: i, j
    found = 0
    do i = 1, 3
        do j = 1, size(a)
            if (a(j) > 2.5) exit
        end do
        found = found + j
        c = a * i
    end do
end subroutine

end module

program realloc_lhs_hoist_01
use realloc_lhs_hoist_01_mod
implicit none
real, allocatable :: a(:), b(:), c(:), d(:), e(:), t(:), m(:,:)
real, allocatable, target :: ct(:)
real, pointer :: p(:)
integer :: i, j, n, k, trips, seen, sizes(4)

! Unallocated target, allocated before the first iteration.
a = [1.0, 2.0, 3.0]
b = [10.0, 20.0, 30.0]
call add(a, b, c, 3)
if (.not. allocated(c)) error stop 1
if (size(c) /= 3) error stop 2
if (any(c /= [14.0, 22.0, 33.0])) error stop 3

! A target with another shape is reallocated.
deallocate(c)
allocate(c(7))
call add(a, b, c, 2)
if (size(c) /= 3) error stop 4
if (any(c /= [16.0, 22.0, 33.0])) error stop 5

! A target with the same shape keeps its bounds.
deallocate(c)
allocate(c(0:2))
call add(a, b, c, 1)
if (lbound(c, 1) /= 0 .or. ubound(c, 1) /= 2) error stop 6
if (any(c /= [17.0, 22.0, 33.0])) error stop 7

! A loop that does not run leaves the target alone.
deallocate(c)
call add(a, b, c, 0)
if (allocated(c)) error stop 8
allocate(c(5))
c = 9
call add(a, b, c, 0)
if (size(c) /= 5) error stop 9
if (any(c /= 9)) error stop 10
n = 0
do i = 1, n
    c = a
end do
if (size(c) /= 5) error stop 11
do i = 1, 3, -1
    c = a
end do
if (size(c) /= 5) error stop 12
k = 0
do i = 3, 1, -1
    k = k + 1
    c = a
end do
if (k /= 3 .or. size(c) /= 3) error stop 13

! The bounds of a reallocated whole-array copy are those of the value.
deallocate(c)
allocate(d(-2:2))
d = [1, 2, 3, 4, 5]
do i = 1, 2
    c = d
end do
if (lbound(c, 1) /= -2 .or. ubound(c, 1) /= 2) error stop 14

! The shape depends on the loop variable.
do i = 1, 4
    c = d(-2:-3 + i)
    sizes(i) = size(c)
end do
if (any(sizes /= [1, 2, 3, 4])) error stop 15

! The shape depends on a variable the loop changes.
n = 1
do i = 1, 4
    n = n + 1
    c = d(-2:-3 + n)
    sizes(i) = size(c)
end do
if (any(sizes /= [2, 3, 4, 5])) error stop 16

! The array the shape is taken from is reallocated in the loop.
do i = 1, 4
    deallocate(a)
    allocate(a(i))
    a = i
    c = a + 1
    sizes(i) = size(c)
end do
if (any(sizes /= [1, 2, 3, 4])) error stop 17
if (any(c /= 5)) error stop 18

! The array the shape is taken from grows in a call.
a = [1.0]
do i = 1, 3
    call grow(a)
    c = a
    sizes(i) = size(c)
end do
if (any(sizes(1:3) /= [2, 3, 4])) error stop 19

! The target is deallocated in the loop.
a = [1.0, 2.0]
do i = 1, 3
    c = a * i
    sizes(i) = size(c)
    deallocate(c)
end do
if (allocated(c)) error stop 20
if (any(sizes(1:3) /= 2)) error stop 21

! The target is moved away in the loop.
do i = 1, 3
    c = a * i
    call move_alloc(c, d)
    if (allocated(c)) error stop 22
end do
if (size(d) /= 2) error stop 23
if (any(d /= [3.0, 6.0])) error stop 24

! The target is reallocated by a call.
do i = 1, 3
    c = a * i
    sizes(i) = size(c)
    call shrink(c)
end do
if (any(sizes(1:3) /= 2)) error stop 25
if (size(c) /= 1) error stop 26

! A module variable target is reallocated by a call that does not name it.
do i = 1, 3
    mc = a * i
    sizes(i) = size(mc)
    call reset_mc()
end do
if (any(sizes(1:3) /= 2)) error stop 27
if (size(mc) /= 1 .or. mc(1) /= -1) error stop 28

! A call changes the values of the target, but not its shape.
do i = 1, 3
    c = a + i
    call scale(c)
end do
if (size(c) /= size(a)) error stop 54
if (any(c /= 2 * (a + 3))) error stop 55

! The loop bounds name the target.
c = [1.0, 2.0]
a = [1.0, 2.0, 3.0, 4.0, 5.0]
call bounds_name_target(a, c, trips)
if (trips /= 2) error stop 29
if (size(c) /= 5) error stop 30

! The loop is left, or the assignment skipped, before the first assignment.
deallocate(c)
call exit_first(a, c, 1)
if (allocated(c)) error stop 31
call exit_first(a, c, 2)
if (size(c) /= 5) error stop 32
deallocate(c)
call cycle_all(a, c)
if (allocated(c)) error stop 33
call return_first(a, c)
if (allocated(c)) error stop 34
call conditional(a, c, .false.)
if (allocated(c)) error stop 35
call conditional(a, c, .true.)
if (size(c) /= 5) error stop 36

! The body reads the target before the assignment.
c = [1.0, 2.0]
call observe_before(a, c, seen)
if (seen /= 7) error stop 37

call inner_exit(a, c, found=k)
if (k /= 9) error stop 38
if (any(c /= 3 * a)) error stop 39

! A pointer to the target is read before the assignment.
ct = [1.0, 2.0, 3.0, 4.0, 5.0]
p => ct
k = 0
do i = 1, 3
    k = k + int(p(1))
    ct = a + i
end do
if (k /= 1 + 2 + 3) error stop 40

! Nested loops.
deallocate(c)
allocate(m(4, 3))
m = 1
do i = 1, 2
    do j = 1, size(m, 2)
        t = m(:, j) * j
        m(1, j) = t(1) + 1
    end do
end do
if (size(t) /= 4) error stop 41
if (any(m(1, :) /= [3.0, 7.0, 13.0])) error stop 42
deallocate(t)
n = 0
do i = 1, 2
    do j = 1, n
        t = m(:, j)
    end do
end do
if (allocated(t)) error stop 43
do i = 1, 2
    do j = 1, 3
        t = b * j
    end do
end do
if (size(t) /= 3) error stop 48
if (any(t /= 3 * b)) error stop 49
n = 0
do i = 1, 2
    do j = 1, 3
        do k = 1, n
            e = b
        end do
    end do
end do
if (allocated(e)) error stop 50

! The body checks the array the shape is read from, or calls a procedure,
! before the assignment.
do i = 1, 2
    if (size(a) < 1) error stop 51
    c = a * 2
end do
if (size(c) /= size(a)) error stop 52
deallocate(c)
do i = 1, 2
    call reset_mc()
    c = a * 2
end do
if (size(c) /= size(a)) error stop 53

! DO WHILE.
k = 0
do while (k < 3)
    k = k + 1
    c = a(1:k)
end do
if (size(c) /= 3) error stop 44
deallocate(c)
do while (k < 3)
    c = a
end do
if (allocated(c)) error stop 45

! DO CONCURRENT.
do concurrent (i = 1:1)
    c = a + 1
end do
if (size(c) /= 5) error stop 46
if (any(c /= a + 1)) error stop 47

print *, "ok"
end program
