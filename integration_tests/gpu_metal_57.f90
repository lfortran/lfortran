module gpu_metal_57_m
  implicit none
  type :: t
  contains
    procedure :: f
  end type
contains
  elemental function f(self, x) result(r)
    class(t), intent(in) :: self
    real, intent(in) :: x
    real :: r
    r = x * 2.0
  end function
end module gpu_metal_57_m

program gpu_metal_57
  use gpu_metal_57_m
  implicit none
  type(t) :: a
  integer :: i
  real :: x(4), y(4)
  x = [1.0, 2.0, 3.0, 4.0]
  y = 0.0
  do concurrent (i = 1:4)
    associate(tmp => x(1))
      y(i) = a%f(x(i))
    end associate
  end do
  if (abs(y(1) - 2.0) > 1e-6) error stop
  if (abs(y(2) - 4.0) > 1e-6) error stop
  if (abs(y(3) - 6.0) > 1e-6) error stop
  if (abs(y(4) - 8.0) > 1e-6) error stop
  print *, "PASS"
end program gpu_metal_57
