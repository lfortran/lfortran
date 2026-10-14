program realloc_lhs_27
! Intrinsic assignment to an allocated allocatable array reallocates it
! when the shape differs, even if the number of elements is the same
implicit none
type :: t
    integer, allocatable :: x(:)
end type
integer, allocatable :: a(:,:), b(:,:)
real, allocatable :: r(:,:)
integer, allocatable :: c(:,:,:)
integer, allocatable :: z(:,:), w(:,:)
type(t), allocatable :: s(:,:), u(:,:)
integer :: i, j, k

allocate(a(2,3))
a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
a = transpose(a)
if (any(shape(a) /= [3, 2])) error stop
if (any(a /= reshape([1, 3, 5, 2, 4, 6], [3, 2]))) error stop
a = transpose(a)
if (any(shape(a) /= [2, 3])) error stop
if (any(a /= reshape([1, 2, 3, 4, 5, 6], [2, 3]))) error stop

b = reshape([10, 20, 30, 40, 50, 60], [3, 2])
a = b
if (any(shape(a) /= [3, 2])) error stop
if (any(a /= b)) error stop

r = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
r = transpose(r)
if (any(shape(r) /= [3, 2])) error stop
if (any(abs(r - reshape([1.0, 3.0, 5.0, 2.0, 4.0, 6.0], [3, 2])) > 1e-6)) error stop

allocate(c(2,3,4))
c = 1
c = reshape([(k, k = 1, 24)], [4, 3, 2])
if (any(shape(c) /= [4, 3, 2])) error stop
if (c(4, 3, 2) /= 24) error stop

! The same shape keeps the allocation and its bounds
deallocate(a)
allocate(a(0:1, -1:1))
b = transpose(b)
a = b
if (any(lbound(a) /= [0, -1])) error stop
if (a(1, 1) /= 60) error stop

! Zero-size arrays with a different shape are reallocated too
allocate(z(0,3), w(3,0))
z = w
if (any(shape(z) /= [3, 0])) error stop

! Allocatable components are deep-copied and the shape is updated
allocate(s(2,3), u(3,2))
do j = 1, 3
    do i = 1, 2
        s(i,j)%x = [i, j]
    end do
end do
do j = 1, 2
    do i = 1, 3
        u(i,j)%x = [100*i, j]
    end do
end do
s = u
if (any(shape(s) /= [3, 2])) error stop
do j = 1, 2
    do i = 1, 3
        if (any(s(i,j)%x /= [100*i, j])) error stop
    end do
end do
u(1,1)%x(1) = -1
if (s(1,1)%x(1) /= 100) error stop
print *, "ok"
end program
