! Test: module-scope parameter referenced inside associate block in function
! called from do concurrent. The gpu_offload pass must correctly import the
! parameter into the duplicated function's scope even when accessed through
! an associate block body.
module gpu_metal_149_m
  implicit none
  integer, parameter :: c = 10
contains
  pure function add_param(x) result(y)
    real, intent(in) :: x
    real :: y
    associate(unused => 1)
      y = x + c
    end associate
  end function
end module

program gpu_metal_149
  use gpu_metal_149_m, only : add_param
  implicit none
  integer :: i
  real :: r(4)
  do concurrent (i = 1:4)
    r(i) = add_param(real(i))
  end do
  if (abs(r(1) - 11.0) > 1e-5) error stop
  if (abs(r(2) - 12.0) > 1e-5) error stop
  if (abs(r(3) - 13.0) > 1e-5) error stop
  if (abs(r(4) - 14.0) > 1e-5) error stop
  print *, "PASS"
end program
