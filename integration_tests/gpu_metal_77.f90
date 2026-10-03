program gpu_metal_77
implicit none
integer :: i
real :: a(2, 2, 2)
a = 1.0

! 2D array section assignment inside do concurrent
do concurrent (i = 1:2)
  a(1:1, 1:1, i) = 0.5
end do

if (abs(a(1,1,1) - 0.5) > 1e-6) error stop
if (abs(a(2,1,1) - 1.0) > 1e-6) error stop
if (abs(a(1,2,1) - 1.0) > 1e-6) error stop
if (abs(a(2,2,1) - 1.0) > 1e-6) error stop
if (abs(a(1,1,2) - 0.5) > 1e-6) error stop
if (abs(a(2,1,2) - 1.0) > 1e-6) error stop
if (abs(a(1,2,2) - 1.0) > 1e-6) error stop
if (abs(a(2,2,2) - 1.0) > 1e-6) error stop

print *, "ok"
end program
