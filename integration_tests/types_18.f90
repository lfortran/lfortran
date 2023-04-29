program types_18

  type(real(kind=4)) :: x

  x = 3.14

  print *, x

  if (abs(x - 3.14) > 1e-12) error stop

end program
