program gpu_metal_136
! Test: submodule function returning derived type in do concurrent
! with --gpu=metal. Verifies that the GPU kernel extraction correctly
! handles ExternalSymbol references for struct types owned by the
! GpuKernelFunction scope.
use gpu_metal_136_m, only: point, make_point
implicit none
type(point) :: pts(4)
integer :: i

do concurrent (i = 1:4)
  pts(i) = make_point(real(i), real(i) * 10.0)
end do

print *, pts(1)%x, pts(1)%y
print *, pts(2)%x, pts(2)%y
print *, pts(3)%x, pts(3)%y
print *, pts(4)%x, pts(4)%y
if (abs(pts(1)%x - 1.0) > 1e-6) error stop
if (abs(pts(1)%y - 10.0) > 1e-6) error stop
if (abs(pts(2)%x - 2.0) > 1e-6) error stop
if (abs(pts(2)%y - 20.0) > 1e-6) error stop
if (abs(pts(3)%x - 3.0) > 1e-6) error stop
if (abs(pts(3)%y - 30.0) > 1e-6) error stop
if (abs(pts(4)%x - 4.0) > 1e-6) error stop
if (abs(pts(4)%y - 40.0) > 1e-6) error stop
end program
