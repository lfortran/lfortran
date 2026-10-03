module gpu_metal_157_m
  implicit none
  type :: tensor_t
    real :: v(2)
  contains
    procedure :: values => get_values
  end type
contains
  pure function get_values(self) result(r)
    class(tensor_t), intent(in) :: self
    real :: r(2)
    r = self%v
  end function
end module

program gpu_metal_157
  use gpu_metal_157_m
  implicit none
  type(tensor_t) :: y(2)
  real :: z(2)
  integer :: pair

  y(1)%v = [0.5, 1.5]
  y(2)%v = [2.5, 3.5]
  z = 0.0

  do concurrent (pair = 1:2)
    associate(yy => y(pair)%values())
      z(pair) = yy(1) + yy(2)
    end associate
  end do

  if (abs(z(1) - 2.0) > 1e-6) error stop
  if (abs(z(2) - 6.0) > 1e-6) error stop
  print *, "PASS"
end program
