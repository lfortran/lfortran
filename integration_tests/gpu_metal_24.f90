program gpu_metal_24
implicit none
type :: params
  real :: scale
  real :: offset
end type
type(params) :: p
real :: a(10)
integer :: i

p%scale = 2.0
p%offset = 0.5
do concurrent(i = 1:10)
  a(i) = p%scale * real(i) + p%offset
end do

if (abs(a(1) - 2.5) > 1e-5) error stop
if (abs(a(5) - 10.5) > 1e-5) error stop
if (abs(a(10) - 20.5) > 1e-5) error stop
print *, "PASSED"
end program
