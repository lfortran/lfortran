module gpu_metal_188_m
  implicit none
  type :: vec_t
    real, allocatable :: v(:)
  end type
contains
  pure function compute(inp) result(s)
    type(vec_t), intent(in) :: inp
    real :: s
    s = sum(inp%v)
  end function
end module

program gpu_metal_188
  use gpu_metal_188_m
  implicit none
  type(vec_t) :: inputs(2)
  real :: results(2)
  integer :: i

  allocate(inputs(1)%v(2))
  inputs(1)%v(1) = 1.0
  inputs(1)%v(2) = 2.0
  allocate(inputs(2)%v(2))
  inputs(2)%v(1) = 3.0
  inputs(2)%v(2) = 4.0

  do concurrent(i=1:2)
    results(i) = compute(inputs(i))
  end do

  print *, results(1)
  print *, results(2)

  if (abs(results(1) - 3.0) > 1e-6) error stop
  if (abs(results(2) - 7.0) > 1e-6) error stop
end program
