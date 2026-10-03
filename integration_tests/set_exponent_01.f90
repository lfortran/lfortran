program set_exponent_01
  implicit none
  real :: x = 178.1387e-4
  integer :: i = 17
  real :: r1, r2

  r1 = set_exponent(x, i)
  r2 = fraction(x) * radix(x)**i

  if (abs(r1 - r2) > 1.0e-3) error stop
  if (abs(r1 - 74716.7891) > 1.0) error stop
end program
