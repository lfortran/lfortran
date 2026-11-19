! Copies back and forth between two allocatable arrays in a loop, as in
! `un = u; ...; u = un`: the first assignment gives both arrays the same
! shape, so the second has nothing to reallocate unless something changes
! the shape of either in between.
module realloc_lhs_hoist_04_mod
implicit none
contains

subroutine grow(x)
    real, allocatable, intent(inout) :: x(:)
    real, allocatable :: y(:)
    allocate(y(size(x) + 1))
    y = 0
    y(1:size(x)) = x
    call move_alloc(y, x)
end subroutine

subroutine relax(u, un, reps)
    real, allocatable, intent(inout) :: u(:), un(:)
    integer, intent(in) :: reps
    integer :: i, n
    n = size(u)
    do i = 1, reps
        un = u
        un(2:n-1) = 0.5 * (u(1:n-2) + u(3:n))
        u = un
    end do
end subroutine

end module

program realloc_lhs_hoist_04
use realloc_lhs_hoist_04_mod
implicit none
real, allocatable :: u(:), un(:), v(:), a2(:,:), b2(:,:)
integer :: i, k

! The stencil, with the copy unallocated and with another size.
u = [0.0, 4.0, 8.0, 4.0, 0.0]
call relax(u, un, 1)
if (size(un) /= 5 .or. size(u) /= 5) error stop 1
if (any(u /= [0.0, 4.0, 4.0, 4.0, 0.0])) error stop 2
deallocate(un)
allocate(un(9))
call relax(u, un, 2)
if (size(un) /= 5) error stop 3
if (any(u /= [0.0, 2.0, 2.0, 2.0, 0.0])) error stop 4

! The bounds of the source are kept.
deallocate(un)
deallocate(u)
allocate(u(0:2))
u = [1.0, 2.0, 3.0]
do i = 1, 2
    un = u
    un(1) = un(1) + 1
    u = un
end do
if (lbound(un, 1) /= 0 .or. lbound(u, 1) /= 0) error stop 5
if (any(u /= [1.0, 4.0, 3.0])) error stop 6

! The source is reallocated in between.
do i = 1, 2
    un = u
    deallocate(u)
    allocate(u(i))
    u = un
end do
if (size(u) /= 3) error stop 7

! The source grows in a call in between.
k = size(u)
do i = 1, 2
    un = u
    call grow(un)
    u = un
end do
if (size(u) /= k + 2) error stop 8

! The copy gets another shape in between.
v = [1.0, 2.0]
do i = 1, 2
    un = u
    un = v
    u = un
end do
if (size(u) /= 2 .or. any(u /= v)) error stop 9

! The loop is left in between.
u = [1.0, 2.0, 3.0]
deallocate(un)
do i = 1, 3
    un = u
    if (i == 1) exit
    u = un
end do
if (size(un) /= 3) error stop 10

! Rank 2.
allocate(a2(2, 3))
a2 = 1
do i = 1, 2
    b2 = a2
    b2(1, 1) = b2(1, 1) + 1
    a2 = b2
end do
if (any(shape(b2) /= [2, 3])) error stop 11
if (a2(1, 1) /= 3) error stop 12

print *, "ok"
end program
