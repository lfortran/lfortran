program gpu_metal_95
! Accessing fixed-size array component of derived type inside do concurrent
implicit none
type :: t
  real :: v(2)
end type
type(t) :: a(2)
real :: r(2, 2)
integer :: i
a(1) = t([1.0, 2.0])
a(2) = t([3.0, 4.0])
do concurrent (i = 1:2)
  r(:, i) = a(i)%v
end do
if (abs(r(1,1) - 1.0) > 1e-6) error stop
if (abs(r(2,1) - 2.0) > 1e-6) error stop
if (abs(r(1,2) - 3.0) > 1e-6) error stop
if (abs(r(2,2) - 4.0) > 1e-6) error stop
print *, "PASSED"
end program
