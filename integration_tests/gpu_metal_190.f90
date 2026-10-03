program gpu_metal_190
implicit none
real :: a(2, 2), r(2, 2)
integer :: i
a = reshape([0.3, 0.7, 0.2, 0.9], [2, 2])
do concurrent(i = 1:2)
  block
    real :: tmp(2)
    tmp = merge(1.0, 0.0, a(:,i) < 0.5)
    r(:,i) = tmp
  end block
end do
if (abs(r(1,1) - 1.0) > 1e-6) error stop
if (abs(r(2,1) - 0.0) > 1e-6) error stop
if (abs(r(1,2) - 1.0) > 1e-6) error stop
if (abs(r(2,2) - 0.0) > 1e-6) error stop
print *, "ok"
end program
