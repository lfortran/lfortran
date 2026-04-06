module gpu_metal_121_m
  implicit none
  type :: activation_t
    integer :: s = 1
  contains
    procedure :: evaluate => real_evaluate
  end type
contains
  elemental function real_evaluate(self, x) result(y)
    class(activation_t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = max(0., x) * self%s
  end function
end module

program gpu_metal_121
  use gpu_metal_121_m
  implicit none
  type(activation_t) :: act
  integer :: pair, n
  real :: a(3), z(3)
  act = activation_t(1)
  n = 3
  z = [1.0, -2.0, 3.0]
  a = 0.0
  do concurrent (pair = 1:2)
    block
      a(1:n) = act%evaluate(z(1:n))
    end block
  end do
  print *, a(1), a(2), a(3)
  if (abs(a(1) - 1.0) > 1e-6) error stop
  if (abs(a(2)) > 1e-6) error stop
  if (abs(a(3) - 3.0) > 1e-6) error stop
end program
