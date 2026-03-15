program equivalence_29
  implicit none

  REAL::SFAC(1),ABC(1)

  EQUIVALENCE      (ABC(1), SFAC(1))
  DATA             SFAC/ -2.50 /
  print *, ABC, SFAC 
  if ( (ABC(1) - 2.5) > 1e-4) error stop
  if ( (SFAC(1) - 2.5) > 1e-4) error stop
end program equivalence_29