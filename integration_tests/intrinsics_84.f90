program intrinsics_84
  real(kind=4) :: x
  real(kind=8) :: y
  integer :: result

  result = minexponent(x)
  print *, result
  if (result /= -125) error stop

  result = minexponent(y)
  print *, result
  if (result /= -1021) error stop

  result = minexponent(4.235)
  print *, result
  if (result /= -125) error stop

  result = minexponent(4.235d0)
  print *, result
  if (result /= -1021) error stop
end program intrinsics_84
