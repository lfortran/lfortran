module bindc_16_mod
  implicit none
  integer, bind(c, name='FORTYTWO') :: FORTYTWO = 42
  real, bind(c, name='PI_APPROX') :: PI_APPROX = 3.14
end module

program bindc_16
  use bindc_16_mod
  implicit none
  if (FORTYTWO /= 42) error stop
  if (abs(PI_APPROX - 3.14) > 1e-5) error stop
  print *, "ok"
end program
