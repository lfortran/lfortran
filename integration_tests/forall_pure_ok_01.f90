module forall_pure_ok_01_m
  implicit none
contains
  pure real function f(i)
    integer, intent(in) :: i
    f = real(i)
  end function
  elemental real function h(i)
    integer, intent(in) :: i
    h = real(i) * 2.0
  end function
end module
program forall_pure_ok_01
  use forall_pure_ok_01_m
  implicit none
  integer :: i
  real :: x(3), y(3)
  forall (i = 1:3) x(i) = f(i)
  forall (i = 1:3) y(i) = h(i) + sqrt(real(i))
  print *, x
  print *, y
  if (any(abs(x - [1.0, 2.0, 3.0]) > 1e-6)) error stop
  if (any(abs(y - [2.0 + 1.0, 4.0 + sqrt(2.0), 6.0 + sqrt(3.0)]) > 1e-5)) error stop
end program
