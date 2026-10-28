! Array assignments for the C/C++ backends: scalar broadcast fills
! the whole array (including 2D and runtime-sized cases), array-to-array
! copies and expressions with broadcasts lower to elementwise loops.
program arrays_135
implicit none
real :: a(3)
real :: b(2, 2)
real, allocatable :: c(:)
real :: d(3), e(3)
real :: f(4), h(4)
integer :: i, j
a = 1.0
do i = 1, 3
    if (abs(a(i) - 1.0) > 1e-6) error stop 1
end do
b = 2.0
do i = 1, 2
    do j = 1, 2
        if (abs(b(i, j) - 2.0) > 1e-6) error stop 2
    end do
end do
allocate(c(4))
c = 3.0
do i = 1, 4
    if (abs(c(i) - 3.0) > 1e-6) error stop 3
end do
d = 1.0
e = d
do i = 1, 3
    if (abs(e(i) - 1.0) > 1e-6) error stop 4
end do
e = d + 2.0
do i = 1, 3
    if (abs(e(i) - 3.0) > 1e-6) error stop 5
end do
f = 0.0
f(:) = 1.0
do i = 1, 4
    if (abs(f(i) - 1.0) > 1e-6) error stop 6
end do
f(2:3) = 2.0
if (abs(f(1) - 1.0) > 1e-6) error stop 7
if (abs(f(2) - 2.0) > 1e-6) error stop 8
if (abs(f(3) - 2.0) > 1e-6) error stop 9
if (abs(f(4) - 1.0) > 1e-6) error stop 10
h = 5.0
f(:) = h + 1.0
do i = 1, 4
    if (abs(f(i) - 6.0) > 1e-6) error stop 11
end do
print *, a(2), e(2)
end program arrays_135
