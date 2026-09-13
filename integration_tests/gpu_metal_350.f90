module gpu_metal_350_mod
implicit none

type vec
    real :: x(3)
end type

type holder
    real, allocatable :: a(:,:)
end type

interface operator(.dot.)
    module procedure dot_rows
end interface

contains

pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function

pure function row_scaled(x) result(y)
    real, intent(in) :: x(:)
    real :: y(size(x))
    y = 2 * x
end function

pure integer function ncols(i)
    integer, intent(in) :: i
    ncols = i + 2
end function

pure real function weighted_sum(x)
    real, intent(in) :: x(:,:)
    integer :: k, l
    weighted_sum = 0
    do l = 1, size(x, 2)
        do k = 1, size(x, 1)
            weighted_sum = weighted_sum + k * l * x(k, l)
        end do
    end do
end function

pure real function dot_rows(x, y)
    real, intent(in) :: x(:), y(:)
    dot_rows = sum(x * y)
end function

! An assumed-shape dummy array whose rows are passed on.
subroutine row_sums(a, s)
    real, intent(in) :: a(:,:)
    real, intent(out) :: s(:)
    integer :: i
    do concurrent (i = 1:size(a, 1))
        s(i) = row_sum(a(i,:))
    end do
end subroutine

end module

program gpu_metal_350
! A row section of an array with run-time extents, `a(i,:)`, passed to a
! device function has to reach it with the stride of its base: its
! elements are `size(a,1)` apart.
use gpu_metal_350_mod
implicit none
real, allocatable :: a(:,:), v(:,:,:), b(:,:)
type(holder) :: h
real :: s(3), d(3), e(3), t(3,5), w(3,2), ref(3), c(5), g(3)
integer :: i, j, k, n

allocate(a(3,5), b(3,5), v(3,4,2))
do i = 1, 3
    do j = 1, 5
        a(i,j) = 10 * i + j
    end do
end do
do i = 1, 3
    ref(i) = sum(a(i,:))
end do

! A scalar result.
do concurrent (i = 1:3)
    s(i) = row_sum(a(i,:))
end do
print *, s
if (any(s /= ref)) error stop

! An array result sized from the dummy.
do concurrent (i = 1:3)
    t(i,:) = row_scaled(a(i,:))
end do
print *, t
if (any(t /= 2 * a)) error stop

! A user-defined operator.
b = a + 1
do concurrent (i = 1:3)
    d(i) = a(i,:) .dot. b(i,:)
end do
print *, d
do i = 1, 3
    if (d(i) /= sum(a(i,:) * b(i,:))) error stop
end do

! An assumed-shape dummy array.
call row_sums(a, e)
print *, e
if (any(e /= ref)) error stop

! The middle index of a 3-D array.
do i = 1, 3
    do j = 1, 4
        do k = 1, 2
            v(i,j,k) = 100 * i + 10 * j + k
        end do
    end do
end do
do concurrent (i = 1:3, k = 1:2)
    w(i,k) = row_sum(v(i,:,k))
end do
print *, w
do i = 1, 3
    do k = 1, 2
        if (w(i,k) /= sum(v(i,:,k))) error stop
    end do
end do

! Leading columns of a component, `h%a(:,1:i)`, are contiguous and are
! passed without a gather.
allocate(h%a(3,5))
h%a = a
do concurrent (i = 1:5)
    c(i) = weighted_sum(h%a(:,1:i))
end do
print *, c
do i = 1, 5
    if (c(i) /= weighted_sum(a(:,1:i))) error stop
end do

! A row whose extent changes with the iteration: the gathered buffer is
! sized as the whole row and the callee gets its leading part.
do concurrent (i = 1:3)
    s(i) = row_sum(a(i,1:i+1))
end do
print *, s
do i = 1, 3
    if (s(i) /= sum(a(i,1:i+1))) error stop
end do

! The same with an extent computed by a function of the index.
do concurrent (i = 1:3)
    g(i) = row_sum(a(i,1:ncols(i)))
end do
print *, g
do i = 1, 3
    if (g(i) /= sum(a(i,1:ncols(i)))) error stop
end do

! The same with an extent held in a local of the loop.
do concurrent (i = 1:3) local(n)
    n = 6 - i
    s(i) = row_sum(a(i,1:n))
end do
print *, s
do i = 1, 3
    if (s(i) /= sum(a(i,1:6-i))) error stop
end do

! Leading columns of fixed height of an allocatable array, which is not
! known to be whole before run time.
do concurrent (i = 1:5)
    c(i) = weighted_sum(a(1:3,1:i))
end do
print *, c
do i = 1, 5
    if (c(i) /= weighted_sum(a(:,1:i))) error stop
end do

end program
