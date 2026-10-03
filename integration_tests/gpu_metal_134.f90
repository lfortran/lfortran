program gpu_metal_134
! Test: submodule function called inside do concurrent with --gpu=metal.
! Verifies that the Metal shader correctly inlines the submodule function
! body instead of generating an empty function.
use gpu_metal_134_m
implicit none
real :: x(4), y(4)
integer :: i

x = [-1.0, 2.0, -3.0, 4.0]
y = 0.0

do concurrent (i = 1:4)
  y(i) = relu(x(i))
end do

print *, y(1), y(2), y(3), y(4)
if (abs(y(1) - 0.0) > 1e-6) error stop
if (abs(y(2) - 2.0) > 1e-6) error stop
if (abs(y(3) - 0.0) > 1e-6) error stop
if (abs(y(4) - 4.0) > 1e-6) error stop
end program
