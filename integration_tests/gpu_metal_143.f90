module gpu_metal_143_m
  implicit none
  type :: t
    real :: s
  contains
    procedure :: f
  end type
contains
  pure real function g(x)
    real, intent(in) :: x
    g = x
  end function
  pure real function f(self, x)
    class(t), intent(in) :: self
    real, intent(in) :: x
    f = g(x * self%s)
  end function
end module

program gpu_metal_143
  use gpu_metal_143_m, only: t
  implicit none
  type(t) :: o
  real :: b(4)
  integer :: i
  o%s = 2.0
  do concurrent(i=1:4)
    b(i) = o%f(real(i))
  end do
  print *, b(1), b(2), b(3), b(4)
  if (abs(b(1) - 2.0) > 1e-6) error stop
  if (abs(b(2) - 4.0) > 1e-6) error stop
  if (abs(b(3) - 6.0) > 1e-6) error stop
  if (abs(b(4) - 8.0) > 1e-6) error stop
end program
