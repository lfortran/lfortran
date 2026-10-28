! Scalar-to-array broadcast for the C/C++ backends: `a = 1.0` fills
! the whole array, including 2D and runtime-sized cases.
program arrays_135
implicit none
real :: a(3)
real :: b(2, 2)
real, allocatable :: c(:)
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
print *, a, b, c
end program arrays_135
