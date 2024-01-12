PROGRAM intrinsics_105
  INTEGER, DIMENSION(-1:1, -1:2, -1:10) :: A
  INTEGER :: result(3)

  result = shape(A)
  print*, shape(A)
  
  if ( result(1) - 3 /= 0 ) error stop
  if ( result(2) - 4 /= 0 ) error stop
  if ( result(3) - 12 /= 0 ) error stop
  if ( size(shape(42)) - 0 /= 0 ) error stop

END PROGRAM
