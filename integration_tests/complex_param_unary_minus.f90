program complex_param_unary_minus
  implicit none
  complex, parameter :: one = 1.0
  complex :: z

  z = -one
  if (abs(real(z) + 1.0) > 1.0e-6 .or. abs(aimag(z)) > 1.0e-6) then
     stop 1
  end if
  print *, 'ok'
end program
