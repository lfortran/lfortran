module gpu_metal_164_m
  implicit none
  type :: t
    integer :: k
  contains
    procedure :: f
  end type
contains
  elemental function f(self, x) result(y)
    class(t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = x + self%k
  end function
end module

program gpu_metal_164
  use gpu_metal_164_m
  implicit none
  type(t) :: a
  integer :: i
  real :: x(2), r(2)
  a = t(1)
  x = 1.0
  r = 0.0
  do concurrent (i = 1:2)
    associate(dummy => 0)
      r = a%f(x)
    end associate
  end do
  if (abs(r(1) - 2.0) > 1e-6) error stop
  if (abs(r(2) - 2.0) > 1e-6) error stop
  print *, "ok"
end program
