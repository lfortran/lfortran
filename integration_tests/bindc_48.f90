program bindc_48
  use bindc_48_mod, only: a_bindc, b_regular, c_regular, d_bindc, e_regular
  implicit none
  a_bindc = 1
  b_regular = 42
  c_regular = 7
  d_bindc = 100
  e_regular = 99
  if (a_bindc /= 1) error stop
  if (b_regular /= 42) error stop
  if (c_regular /= 7) error stop
  if (d_bindc /= 100) error stop
  if (e_regular /= 99) error stop
  print *, a_bindc, b_regular, c_regular, d_bindc, e_regular
end program
