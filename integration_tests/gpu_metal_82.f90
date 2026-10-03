program gpu_metal_82
! Whole-array scalar assignment inside do concurrent
implicit none
integer :: i
real :: x(4)
real :: y(2, 3)
integer :: a(3)

! 1D array broadcast
do concurrent (i = 1:1)
    x = 2.5
end do
if (abs(x(1) - 2.5) > 1.0e-5) error stop
if (abs(x(2) - 2.5) > 1.0e-5) error stop
if (abs(x(3) - 2.5) > 1.0e-5) error stop
if (abs(x(4) - 2.5) > 1.0e-5) error stop

! 2D array broadcast
do concurrent (i = 1:1)
    y = 3.0
end do
if (abs(y(1,1) - 3.0) > 1.0e-5) error stop
if (abs(y(2,1) - 3.0) > 1.0e-5) error stop
if (abs(y(1,2) - 3.0) > 1.0e-5) error stop
if (abs(y(2,2) - 3.0) > 1.0e-5) error stop
if (abs(y(1,3) - 3.0) > 1.0e-5) error stop
if (abs(y(2,3) - 3.0) > 1.0e-5) error stop

! Integer array broadcast
do concurrent (i = 1:1)
    a = 7
end do
if (a(1) /= 7) error stop
if (a(2) /= 7) error stop
if (a(3) /= 7) error stop

print *, "PASSED"
end program
