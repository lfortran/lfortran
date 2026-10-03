program gpu_metal_112
  implicit none
  integer, parameter :: n = 4
  real :: a(n, 2), z(n, 2)
  integer :: i, l

  z = 2.0
  a = 0.0

  do concurrent (i = 1:2)
    do l = 1, 2
      a(1:n, l) = f(z(1:n, l))
    end do
  end do

  if (abs(a(1,1) - 3.0) > 1e-6) error stop
  if (abs(a(2,1) - 3.0) > 1e-6) error stop
  if (abs(a(3,1) - 3.0) > 1e-6) error stop
  if (abs(a(4,1) - 3.0) > 1e-6) error stop
  if (abs(a(1,2) - 3.0) > 1e-6) error stop
  if (abs(a(4,2) - 3.0) > 1e-6) error stop
  print *, "PASSED"

contains
  elemental function f(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x + 1.0
  end function
end program
