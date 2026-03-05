! Test: parameterized derived type with kind parameter
! Ensures tensor_t can be instantiated and assigned correctly

module kind_parameters_m
  implicit none
  integer, parameter :: default_real = kind(1.)
end module kind_parameters_m

module tensor_m
  use kind_parameters_m, only : default_real
  implicit none

  type tensor_t(k)
    integer, kind :: k = default_real
    real(k), allocatable :: values_(:)
  end type tensor_t

end module tensor_m

program tensor_test
  use tensor_m
  use kind_parameters_m, only : default_real
  implicit none

  type(tensor_t(default_real)) :: a, b

  allocate(a%values_(3))
  a%values_ = [1.0_default_real, 2.0_default_real, 3.0_default_real]

  ! intrinsic assignment
  b = a

  if (.not. allocated(b%values_)) error stop
  if (size(b%values_) /= 3) error stop
  if (b%values_(1) /= 1.0_default_real) error stop
  if (b%values_(2) /= 2.0_default_real) error stop
  if (b%values_(3) /= 3.0_default_real) error stop

  print *, "PASS"
end program tensor_test