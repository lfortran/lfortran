! Test: calling an internal function (in contains) with a derived type
! argument containing an allocatable array member, from do concurrent.
! Verifies that the Metal codegen correctly passes allocatable member
! data pointers when emitting function/subroutine calls with struct-typed
! arguments inside GPU kernels.
program gpu_metal_92
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
  integer :: i
  type(tensor_t) :: arr(4)
  real :: out(4)

  arr(1) = tensor_t([10.0])
  arr(2) = tensor_t([20.0])
  arr(3) = tensor_t([30.0])
  arr(4) = tensor_t([40.0])

  do concurrent (i = 1:4)
    out(i) = get_first(arr(i))
  end do

  if (abs(out(1) - 10.0) > 1e-5) error stop
  if (abs(out(2) - 20.0) > 1e-5) error stop
  if (abs(out(3) - 30.0) > 1e-5) error stop
  if (abs(out(4) - 40.0) > 1e-5) error stop
  print *, "PASS"
contains
  pure function get_first(x) result(r)
    type(tensor_t), intent(in) :: x
    real :: r
    r = x%values_(1)
  end function
end program
