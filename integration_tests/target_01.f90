program target_01
  implicit none
  real, target :: x = 5.0
  real, pointer :: p

  p => x
  if (p /= 5.0) error stop
  
  print *, "target variable successfully implemented"
end program target_01
