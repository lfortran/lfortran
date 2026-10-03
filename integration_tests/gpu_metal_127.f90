program gpu_metal_127
! Test: pure function returning a derived-type array called inside
! do concurrent. Verifies that the Metal backend emits the function
! definition once (not duplicated) and passes array parameters correctly.
implicit none
type :: t
  real :: v
end type
type(t) :: a(2)
real :: res(2)
integer :: i

do concurrent (i = 1:2)
  a = f()
  res(i) = a(i)%v
end do
if (abs(res(1) - 1.0) > 1e-6) error stop
if (abs(res(2) - 2.0) > 1e-6) error stop
print *, "PASSED"

contains
  pure function f() result(r)
    type(t) :: r(2)
    r(1)%v = 1.0
    r(2)%v = 2.0
  end function
end program
