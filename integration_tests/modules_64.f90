MODULE xmod
  REAL(kind(1d0)) :: x = -666
END MODULE xmod

PROGRAM modules_64
  USE xmod                      
  USE xmod, ONLY: xrenamed => x 
  REAL :: x                     
  
  x = 666                       
  
  if (kind(xrenamed) == kind(x)) error stop
  if (xrenamed /= -666) error stop
  if (x /= 666) error stop
  if (kind(xrenamed) /= 8) error stop
  if (kind(x) /= 4) error stop
END PROGRAM modules_64
