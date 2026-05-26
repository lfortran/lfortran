program separate_compilation_50
  use separate_compilation_50_mod_a
  use separate_compilation_50_mod_b
  real :: x, y
  x = 1.0
  y = 2.0
  call sc50a(x)
  call sc50b(y)
  if (abs(x - 0.841470957) > 1.0e-5) error stop
  if (abs(y - 0.909297407) > 1.0e-5) error stop
  print *, x, y
end program
