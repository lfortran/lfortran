program floor_02
  implicit none
  real, parameter :: a1 = 3.3 ! 3
  real, parameter :: a2 = 3.5 ! 3
  real, parameter :: a3 = 3.7 ! 3
  real, parameter :: b1 = -3.3 ! -4
  real, parameter :: b2 = -3.5 ! -4
  real, parameter :: b3 = -3.7 ! -4
  integer, parameter :: x1 = floor(a1) ! 3
  integer, parameter :: x2 = floor(a2) ! 3
  integer, parameter :: x3 = floor(a3) ! 3
  integer, parameter :: y1 = floor(b1) ! -4
  integer, parameter :: y2 = floor(b2) ! -4
  integer, parameter :: y3 = floor(b3) ! -4
  
  if(x1 /= 3 .or. x2 /= 3 .or. x3 /= 3) error stop
  if(y1 /= -4 .or. y2 /= -4 .or. y3 /= -4) error stop
  
  print*, x1, x2, x3
  print*, y1, y2, y3
end program
