module gpu_metal_192_m
  implicit none
  type :: t
    real :: v
  contains
    procedure :: get_v
  end type
contains
  pure function get_v(self) result(r)
    class(t), intent(in) :: self
    real :: r
    r = self%v
  end function
end module

program gpu_metal_192
  use gpu_metal_192_m
  implicit none
  integer :: i
  type(t) :: a(4)
  real :: b(4)

  a = [t(1.0), t(2.0), t(3.0), t(4.0)]

  do concurrent(i=1:4)
    b(i) = f(a(i))
  end do

  print *, b
  if (any(abs(b - [1.0, 2.0, 3.0, 4.0]) > 1.0e-6)) error stop

contains
  pure function f(x) result(r)
    type(t), intent(in) :: x
    real :: r
    r = x%get_v()
  end function
end program
