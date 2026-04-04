module gpu_metal_26_m
  implicit none
  type :: inner_t
  contains
    procedure :: eval
  end type
  type :: outer_t
    type(inner_t) :: f
  end type
contains
  elemental function eval(self, x) result(y)
    class(inner_t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = x * 2.0
  end function
end module

program gpu_metal_26
  use gpu_metal_26_m
  implicit none
  type(outer_t) :: o
  real :: x(4), y(4)
  integer :: i
  x = [1.0, 2.0, 3.0, 4.0]
  do concurrent (i = 1:4)
    y(i) = o%f%eval(x(i))
  end do
  if (abs(y(1) - 2.0) > 1e-5) error stop
  if (abs(y(2) - 4.0) > 1e-5) error stop
  if (abs(y(3) - 6.0) > 1e-5) error stop
  if (abs(y(4) - 8.0) > 1e-5) error stop
  print *, "PASSED"
end program
