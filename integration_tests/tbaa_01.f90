! TRANSFER puns in loops that also store to the arrays involved, including
! a derived type with a pointer component transferred to integers and back
! and then written through.
module tbaa_01_m
implicit none

type :: holder_t
    integer :: n = 0
    real, pointer :: p => null()
end type

type :: pts_t
    real :: x, y
end type

contains

subroutine bump_bits(x, k)
    real, allocatable, intent(inout) :: x(:)
    integer, allocatable, intent(inout) :: k(:)
    integer :: i
    do i = 1, size(x)
        k(i) = transfer(x(i), k(i))
        k(i) = k(i) + 1
        x(i) = transfer(k(i), x(i))
    end do
end subroutine

subroutine through_integers(h, t, buf)
    type(holder_t), intent(inout) :: h(:)
    real, target, intent(inout) :: t(:)
    integer, intent(inout) :: buf(:)
    type(holder_t) :: h2(1)
    integer :: i
    do i = 1, size(t)
        h(1)%p => t(i)
        buf = transfer(h, buf)
        h2 = transfer(buf, h2)
        h2(1)%p = h2(1)%p + h2(1)%n*i
        h(1)%n = h(1)%n + 1
    end do
end subroutine

subroutine points_to_reals(pts, r)
    type(pts_t), allocatable, intent(inout) :: pts(:)
    real, allocatable, intent(inout) :: r(:)
    integer :: i
    do i = 1, size(pts)
        r = transfer(pts, r)
        r(2*i) = r(2*i) + r(2*i-1)
        pts = transfer(r, pts)
    end do
end subroutine

end module

program tbaa_01
use tbaa_01_m
implicit none
real, allocatable :: x(:)
integer, allocatable :: k(:), buf(:)
type(holder_t) :: h(1)
real, allocatable, target :: t(:)
type(pts_t), allocatable :: pts(:)
real, allocatable :: r(:)
integer :: i

allocate(x(5), k(5))
x = [1.0, 2.0, 3.0, 4.0, 5.0]
k = 0
call bump_bits(x, k)
do i = 1, 5
    if (x(i) /= nearest(real(i), 1.0)) error stop 1
    if (k(i) /= transfer(nearest(real(i), 1.0), 0)) error stop 2
end do

allocate(t(4))
t = 10
h(1)%n = 1
allocate(buf(size(transfer(h, [0]))))
call through_integers(h, t, buf)
print *, t, h(1)%n
if (any(abs(t - [11.0, 14.0, 19.0, 26.0]) > 1e-6)) error stop 3
if (h(1)%n /= 5) error stop 4
if (.not. associated(h(1)%p, t(4))) error stop 7

allocate(pts(3))
pts = [pts_t(1, 2), pts_t(3, 4), pts_t(5, 6)]
allocate(r(6))
call points_to_reals(pts, r)
print *, pts
if (any(abs(pts%y - [3.0, 7.0, 11.0]) > 1e-6)) error stop 5
if (any(abs(pts%x - [1.0, 3.0, 5.0]) > 1e-6)) error stop 6
print *, "ok"
end program
