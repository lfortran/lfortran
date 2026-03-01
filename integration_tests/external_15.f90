! externalfun.f90
program external_fun
  implicit none
  character(8), external:: sin
  character(8):: result
  result = sin()
  if (result /= 'Peccavi!') stop 1
  print "(A)", result
  print *, 'Test passed'
end program external_fun

character(8) function sin()
  sin = 'Peccavi!'
end function sin