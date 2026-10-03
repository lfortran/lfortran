module gpu_metal_58_m
  implicit none
  type :: t
  contains
    procedure :: f
  end type
contains
  elemental real function f(self, x)
    class(t), intent(in) :: self
    real, intent(in) :: x
    f = x + 1.0
  end function
end module gpu_metal_58_m

program gpu_metal_58
  use gpu_metal_58_m
  implicit none
  type(t) :: o
  integer :: i
  real :: x(4), y(4)
  x = [1.0, 2.0, 3.0, 4.0]
  y = 0.0
  associate(a => 1)
    do concurrent (i = 1:4)
      y(i) = o%f(x(i))
    end do
  end associate
  if (abs(y(1) - 2.0) > 1e-6) error stop
  if (abs(y(2) - 3.0) > 1e-6) error stop
  if (abs(y(3) - 4.0) > 1e-6) error stop
  if (abs(y(4) - 5.0) > 1e-6) error stop
  print *, "PASS"
end program gpu_metal_58
