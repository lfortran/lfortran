program gpu_metal_52
implicit none
integer :: n(2)
real :: b(3,2)
integer :: l

n = [2, 3]
b = 0.0

do concurrent(l = 1:2)
  b(1:n(l),l) = 1.0
end do

if (abs(b(1,1) - 1.0) > 0.01) error stop
if (abs(b(2,1) - 1.0) > 0.01) error stop
if (abs(b(3,1) - 0.0) > 0.01) error stop
if (abs(b(1,2) - 1.0) > 0.01) error stop
if (abs(b(2,2) - 1.0) > 0.01) error stop
if (abs(b(3,2) - 1.0) > 0.01) error stop
print *, "ok"
end program
