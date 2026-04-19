module gpu_metal_184_m
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
  interface tensor_t
    module procedure construct
  end interface
contains
  pure function construct(values) result(tensor)
    real, intent(in) :: values(:)
    type(tensor_t) :: tensor
    tensor%values_ = values
  end function
end module

program gpu_metal_184
  use gpu_metal_184_m
  implicit none
  type(tensor_t) :: inputs(1), outputs(1)
  integer :: i

  ! Test true branch: sum([1.0, 1.0]) = 2.0 > 0.99
  inputs(1) = tensor_t([1.0, 1.0])
  do concurrent(i = 1:1)
    outputs(i) = tensor_t([merge(1.0, 0.0, sum(inputs(i)%values_) > 0.99)])
  end do
  if (abs(outputs(1)%values_(1) - 1.0) > 1e-6) error stop "FAIL: expected 1.0"

  ! Test false branch: sum([0.5, 0.3]) = 0.8 < 0.99
  inputs(1) = tensor_t([0.5, 0.3])
  do concurrent(i = 1:1)
    outputs(i) = tensor_t([merge(1.0, 0.0, sum(inputs(i)%values_) > 0.99)])
  end do
  if (abs(outputs(1)%values_(1) - 0.0) > 1e-6) error stop "FAIL: expected 0.0"

  print *, "PASS"
end program
