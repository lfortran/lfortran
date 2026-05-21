program format_102
  implicit none
  character(len=12) :: s
  real :: x

  ! Test G format with scale factor 1P (E-mode)
  x = 987.654
  write(s, '(1PG12.2)') x
  if (s /= "    9.88E+02") error stop

  ! Test G format with scale factor 1P, smaller value (E-mode)
  x = 0.001234
  write(s, '(1PG12.4)') x
  if (s /= "  1.2340E-03") error stop

  ! Test G format with no scale factor (default, E-mode)
  x = 987.654
  write(s, '(G12.2)') x
  if (s /= "    0.99E+03") error stop

  ! Test G format with scale factor 1P, value in F-mode range
  x = 3.5
  write(s, '(1PG12.4)') x
  if (s /= "   3.500    ") error stop

  print *, "All tests passed."
end program
