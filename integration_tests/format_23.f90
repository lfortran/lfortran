program format_23
  implicit none
  real(4) :: x1, y1, z1
  real(8) :: x8, y8, z8

  x1 = 3.14
  y1 = -2.5
  z1 = 0.0

  x8 = 3.14_8
  y8 = -2.5_8
  z8 = 0.0_8

  print *, "Binary format for real(4):"
  print '(a, b0)', "x1 (3.14): ", x1
  print '(a, b10)', "x1 (3.14): ", x1
  print '(a, b32)', "x1 (3.14): ", x1

  print '(a, b0)', "y1 (-2.5): ", y1
  print '(a, b10)', "y1 (-2.5): ", y1
  print '(a, b32)', "y1 (-2.5): ", y1

  print '(a, b0)', "z1 (0.0): ", z1
  print '(a, b32)', "z1 (0.0): ", z1

  print *, "Binary format for real(8):"
  print '(a, b0)', "x8 (3.14): ", x8
  print '(a, b10)', "x8 (3.14): ", x8
  print '(a, b64)', "x8 (3.14): ", x8

  print '(a, b0)', "y8 (-2.5): ", y8
  print '(a, b10)', "y8 (-2.5): ", y8
  print '(a, b64)', "y8 (-2.5): ", y8

  print '(a, b0)', "z8 (0.0): ", z8
  print '(a, b64)', "z8 (0.0): ", z8

end program format_23
