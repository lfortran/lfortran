program floor_01
  implicit none
  real, parameter :: x1 = 3.3 ! 3
  real, parameter :: x2 = 3.5 ! 3
  real, parameter :: x3 = 3.7 ! 3
  real, parameter :: y1 = -3.3 ! -4
  real, parameter :: y2 = -3.5 ! -4
  real, parameter :: y3 = -3.7 ! -4

  if(floor(x1, kind=8) /= 3_8 .or. floor(x2) /= 3 .or. floor(x3) /= 3) error stop
  if(floor(y1, kind=4) /= -4 .or. floor(y2) /= -4 .or. floor(y3, kind=8) /= -4) error stop

  print*, floor(x1), floor(x2), floor(x3)
  print*, floor(y1), floor(y2), floor(y3)
end program
