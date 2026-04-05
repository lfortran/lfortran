program gpu_metal_72
implicit none
type :: t
  real, allocatable :: a(:)
end type
type(t) :: x
real :: r(4)
integer :: i

allocate(x%a(4))
x%a = [10.0, 20.0, 30.0, 40.0]

do concurrent(i = 1:4)
  r(i) = x%a(i) * 2.0
end do

if (abs(r(1) - 20.0) > 1e-5) error stop
if (abs(r(2) - 40.0) > 1e-5) error stop
if (abs(r(3) - 60.0) > 1e-5) error stop
if (abs(r(4) - 80.0) > 1e-5) error stop
print *, "PASSED"
end program
