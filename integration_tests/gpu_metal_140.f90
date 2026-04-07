program gpu_metal_140
! Test: type-bound procedure in a submodule called from do concurrent
! with --gpu=metal --separate-compilation. Verifies that the Metal
! shader includes the submodule function body for struct methods.
use gpu_metal_140_m, only: transform_t
implicit none
type(transform_t) :: t
real :: x(4), y(4)
integer :: i

x = [1.0, 2.0, 3.0, 4.0]
y = 0.0

do concurrent (i = 1:4)
  block
    y(i) = t%apply(x(i))
  end block
end do

print *, y
if (abs(y(1) - 2.0) > 1e-6) error stop
if (abs(y(2) - 4.0) > 1e-6) error stop
if (abs(y(3) - 6.0) > 1e-6) error stop
if (abs(y(4) - 8.0) > 1e-6) error stop
end program
