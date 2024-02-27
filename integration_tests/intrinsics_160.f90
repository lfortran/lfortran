program intrinsics_160
    integer :: a1 = 5
    integer :: a2 = 8
    integer :: a3 = -1
    integer :: a4 = -4
    integer :: a5 = -2
    integer :: a6 = -5
  
    print*, ior(5, 8)
    if (ior(5, 8) /= 13) error stop
    print*, ior(-1, 5)
    if (ior(-1, 5) /= -1) error stop
    print*, ior(8, -4)
    if (ior(8, -4) /= -4) error stop
    print*, ior(-2, -5)
    if (ior(-2, -5) /= -1) error stop
  
    print*, ior(a1, a2)
    if (ior(a1, a2) /= 13) error stop
    print*, ior(a3, a1)
    if (ior(a3, a1) /= -1) error stop
    print*, ior(a2, a4)
    if (ior(a2, a4) /= -4) error stop
    print*, ior(a5, a6)
    if (ior(a5, a6) /= -1) error stop
  
  end program
  
  