program types_19
  type(integer) :: a
  type(real) :: b
  a = 25
  b = 3.14
  print *, a, B
  if (a /= 25) error stop
  if (abs(b - 3.14) > 1e-12) error stop
end program
