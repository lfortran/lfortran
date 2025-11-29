program assigned_fmt
  implicit none
  integer :: i

  ASSIGN 0012 TO I
0012 FORMAT (" **** ASSIGN FORMAT NUMBER TO INTEGER VARIABLE ****" )
  WRITE (6, I)

end program
