program gpu_metal_117
! Test that float literals in max/min inside do concurrent are
! correctly emitted in Metal codegen (not truncated to integers).
implicit none
real :: x(4)
integer :: i
x(1) = -1.0
x(2) = 0.5
x(3) = -3.0
x(4) = 2.0
do concurrent (i = 1:4)
    x(i) = max(0.0, x(i))
end do
if (abs(x(1)) > 1e-6) error stop
if (abs(x(2) - 0.5) > 1e-6) error stop
if (abs(x(3)) > 1e-6) error stop
if (abs(x(4) - 2.0) > 1e-6) error stop

x(1) = 5.0
x(2) = 0.5
x(3) = 3.0
x(4) = -1.0
do concurrent (i = 1:4)
    x(i) = min(1.0, x(i))
end do
if (abs(x(1) - 1.0) > 1e-6) error stop
if (abs(x(2) - 0.5) > 1e-6) error stop
if (abs(x(3) - 1.0) > 1e-6) error stop
if (abs(x(4) - (-1.0)) > 1e-6) error stop

print *, "PASS"
end program
