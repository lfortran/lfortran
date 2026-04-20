program bindc_48
  use bindc_48_mod, only: b_regular
  implicit none
  b_regular = 42
  if (b_regular /= 42) error stop
  print *, b_regular
end program
