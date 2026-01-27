program intrinsics_403
  implicit none

  real :: r1, r2, result
  integer :: i_result

  r1 = 42.1
  r2 = 43.43
  i_result = min1(r1, r2)
  print *, 'min1 test: ', i_result
  if (i_result /= 42) error stop

  i_result = max1(r1, r2)
  print *, 'max1 test: ', i_result
  if (i_result /= 43) error stop

  r1 = -5.7
  r2 = -3.2
  i_result = min1(r1, r2)
  print *, 'min1 negative test: ', i_result
  if (i_result /= -5) error stop

  i_result = max1(r1, r2)
  print *, 'max1 negative test: ', i_result
  if (i_result /= -3) error stop

  i_result = min1(10.5, 20.3, 5.1, 15.9)
  print *, 'min1 multiple args: ', i_result
  if (i_result /= 5) error stop

  i_result = max1(10.5, 20.3, 5.1, 15.9)
  print *, 'max1 multiple args: ', i_result
  if (i_result /= 20) error stop

  result = min1(r1, r2)
  print *, 'min1 to real: ', result
  if (result /= -5.0) error stop

  result = max1(r1, r2)
  print *, 'max1 to real: ', result
  if (result /= -3.0) error stop

end program intrinsics_403
