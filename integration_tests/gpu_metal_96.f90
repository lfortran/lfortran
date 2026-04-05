program gpu_metal_96
! Array-section division by scalar inside do concurrent
implicit none
real :: a(4), b(4)
integer :: i

b(1) = 4.0
b(2) = 6.0
b(3) = 10.0
b(4) = 20.0

do concurrent (i = 1:1)
    a(1:4) = b(1:4) / 2
end do

if (abs(a(1) - 2.0) > 1e-6) error stop
if (abs(a(2) - 3.0) > 1e-6) error stop
if (abs(a(3) - 5.0) > 1e-6) error stop
if (abs(a(4) - 10.0) > 1e-6) error stop
print *, "PASSED"
end program
