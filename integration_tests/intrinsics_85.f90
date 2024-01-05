program intrinsics_85
  real(kind=4) :: x
  real(kind=8) :: y
  integer :: result

  result = maxexponent(x)
  print *, 'maxexponent(x) = ', result
  if (result /= 128) error stop

  result = maxexponent(y)
  print *, 'maxexponent(y) = ', result
  if (result /= 1024) error stop

  result = maxexponent(-214.91)
  print *, 'maxexponent(-214.91) = ', result
  if (result /= 128) error stop

  result = maxexponent(214.91D0)
  print *, 'maxexponent(214.91D0) = ', result
  if (result /= 1024) error stop
end program intrinsics_85
