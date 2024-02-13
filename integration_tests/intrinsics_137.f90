program intrinsic_137
    real :: x = 178.1387e-4
    integer :: i = 5
    real :: y = -186.3245e-4
    integer :: j = 3
  
    print *, scale(y,j)
    if (abs(scale(y, j) - (-1.49059594e-01)) > 1e-7) error stop
  
    print *, scale(-186.3245e-4,3)
    if (abs(scale(-186.3245e-4,3) - (-1.49059594e-01)) > 1e-7) error stop
  
    print *, scale(x,i)
    if (abs(scale(x, i) - 5.70043862e-01) > 1e-7) error stop
  
    print *, scale(178.1387e-4,5)
    if (abs(scale(178.1387e-4, 5) - 5.70043862e-01) > 1e-7) error stop
  
  end program 
  