module gpu_metal_114_m
  implicit none
  type :: t
  contains
    procedure :: eval => my_eval
  end type
contains
  elemental function my_eval(self, x) result(y)
    class(t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = x * 2.0
  end function
end module

program gpu_metal_114
  use gpu_metal_114_m
  implicit none
  type(t) :: obj
  integer :: i
  real :: z(3), a(3)
  z = [1.0, 2.0, 3.0]
  a = 0.0

  do concurrent (i = 1:1)
    a = obj%eval(z)
  end do

  if (abs(a(1) - 2.0) > 1e-6) error stop
  if (abs(a(2) - 4.0) > 1e-6) error stop
  if (abs(a(3) - 6.0) > 1e-6) error stop
  print *, a
end program
