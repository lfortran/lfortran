program floor_03
  implicit none
  real :: x1, x2, x3
  real :: y1, y2, y3
  x1 = 3.3 ! 3
  x2 = 3.5 ! 3
  x3 = 3.7 ! 3
  y1 = -3.3 ! -4
  y2 = -3.5 ! -4
  y3 = -3.7 ! -4
    
  if(floor(x1) /= 3 .or. floor(x2) /= 3 .or. floor(x3) /= 3) error stop
  if(floor(y1) /= -4 .or. floor(y2) /= -4 .or. floor(y3) /= -4) error stop

  print*, floor(x1), floor(x2), floor(x3)
  print*, floor(y1), floor(y2), floor(y3)
end program
