! Explicit-shape dummy arguments, TARGET ones included, whose elements are
! written in loops that also read module allocatable arrays: the elements are
! array data, so a store to them does not change a descriptor, but they may
! still alias other arrays and pointers.
module tbaa_14_m
implicit none
real, allocatable :: w(:)
real(8), allocatable :: d(:)
contains

subroutine grow()
    real, allocatable :: t(:)
    allocate(t(size(w)))
    t = 2*w
    call move_alloc(t, w)
end subroutine

! The descriptor of w is invariant: the stores to x do not change it.
subroutine scale_target(n, x)
    integer, intent(in) :: n
    real(8), target, intent(inout) :: x(n)
    integer :: i
    do concurrent (i=1:n)
        x(i) = w(i)*x(i)
    end do
end subroutine

! The actual argument is the allocatable array d itself.
subroutine scale_same(n, x)
    integer, intent(in) :: n
    real(8), target, intent(inout) :: x(n)
    integer :: i
    do i = 1, n
        x(i) = d(i) + x(i)
    end do
end subroutine

! A call in the loop reallocates w, so the descriptor must be read again.
subroutine scale_realloc(n, x)
    integer, intent(in) :: n
    real(8), target, intent(inout) :: x(n)
    integer :: i
    do i = 1, n
        x(i) = w(i)*x(i)
        if (i == 2) call grow()
    end do
end subroutine

! A pointer to the dummy argument writes the elements the loop reads.
subroutine through_pointer(n, x)
    integer, intent(in) :: n
    real(8), target, intent(inout) :: x(n)
    real(8), pointer :: p(:)
    integer :: i
    p => x(2:n)
    do i = 1, n - 1
        p(i) = p(i) + w(i)
        x(i+1) = 2*x(i+1)
    end do
end subroutine
end module

program tbaa_14
use tbaa_14_m
implicit none
real(8) :: x(6), y(6)
integer :: i
allocate(w(6), d(6))

w = [(real(i), i = 1, 6)]
x = 1
call scale_target(6, x)
if (any(abs(x - [1d0, 2d0, 3d0, 4d0, 5d0, 6d0]) > 1d-6)) error stop 1

d = [(real(i, 8), i = 1, 6)]
call scale_same(6, d)
if (any(abs(d - [2d0, 4d0, 6d0, 8d0, 10d0, 12d0]) > 1d-6)) error stop 2

w = [(real(i), i = 1, 6)]
x = 1
call scale_realloc(6, x)
! w(1), w(2) are read before the reallocation, w(3:6) after it (doubled).
if (any(abs(x - [1d0, 2d0, 6d0, 8d0, 10d0, 12d0]) > 1d-6)) error stop 3
if (any(abs(w - [2.0, 4.0, 6.0, 8.0, 10.0, 12.0]) > 1e-6)) error stop 4

w = [(real(i), i = 1, 6)]
y = 1
call through_pointer(6, y)
! Step i: y(i+1) = y(i+1) + w(i), then y(i+1) = 2*y(i+1).
if (any(abs(y - [1d0, 4d0, 6d0, 8d0, 10d0, 12d0]) > 1d-6)) error stop 5
print *, "ok"
end program
