program intrinsics_161
    integer :: a1 = 5  
    integer :: a2 = 8
    integer :: a3 = -1
    integer :: a4 = -4
    integer :: a5 = -2
    integer :: a6 = -5
  
    print*, ieor(5, 8)
    if (ieor(5, 8) /= 13) error stop
    print*, ieor(-1, 5)
    if (ieor(-1, 5) /= -6) error stop
    print*, ieor(8, -4)
    if (ieor(8, -4) /= -12) error stop
    print*, ieor(-2, -5)
    if (ieor(-2, -5) /= 5) error stop
  
    print*, ieor(a1, a2)
    if (ieor(a1, a2) /= 13) error stop
    print*, ieor(a3, a1)
    if (ieor(a3, a1) /= -6) error stop
    print*, ieor(a2, a4)
    if (ieor(a2, a4) /= -12) error stop
    print*, ieor(a5, a6)
    if (ieor(a5, a6) /= 5) error stop
  
  end program 
  