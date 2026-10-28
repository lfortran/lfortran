! lbound/ubound with a dim argument for the C/C++ backends: bounds
! come from the array descriptor.
program arrays_136
implicit none
real :: a(3)
real :: b(2, 2)
real, allocatable :: c(:)
integer :: i, j
do i = 1, 3
    a(i) = real(i)
end do
if (lbound(a, 1) /= 1) error stop 1
if (ubound(a, 1) /= 3) error stop 2
do i = 1, 2
    do j = 1, 2
        b(i, j) = real(i * 10 + j)
    end do
end do
if (lbound(b, 1) /= 1) error stop 3
if (ubound(b, 1) /= 2) error stop 4
if (lbound(b, 2) /= 1) error stop 5
if (ubound(b, 2) /= 2) error stop 6
allocate(c(4))
do i = 1, 4
    c(i) = real(i)
end do
if (lbound(c, 1) /= 1) error stop 7
if (ubound(c, 1) /= 4) error stop 8
print *, lbound(a, 1), ubound(a, 1)
end program arrays_136
