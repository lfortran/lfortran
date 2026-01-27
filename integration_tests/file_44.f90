program scale_factor2
   implicit none

   character(27) :: f_string = ' 13.1234  13.1234E0 1312.34'
   real :: a, b, c
   real :: correct = 13.1234

   read (f_string, '(F9.5, 1X, E9.3, 1X, 2PF7.4)') a, b, c
   print *, 'a, b, c =', a, b, c
   if (abs (correct - a) > 0.001) error stop
   if (abs (correct - b) > 0.001) error stop
   if (abs (correct - c) > 0.001) error stop

end program
