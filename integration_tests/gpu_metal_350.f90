module gpu_metal_350_mod
implicit none

type vec
    real :: x(3)
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
real :: s(3), d(3), e(3), t(3,5), w(3,2), ref(3)
integer :: i, j, k

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

end program
