program intrinsics_164
    integer :: a1 = 5  
    integer :: a2 = 8
    integer :: a3 = -1
    integer :: a4 = -4
    integer :: a5 = -2
    integer :: a6 = -5
  
    print*, iand(5, 8)
    if (iand(5, 8) /= 0) error stop
    print*, iand(-1, 5)
    if (iand(-1, 5) /= 5) error stop
    print*, iand(8, -4)
    if (iand(8, -4) /= 8) error stop
    print*, iand(-2, -5)
    if (iand(-2, -5) /= -6) error stop
  
    print*, iand(a1, a2)
    if (iand(a1, a2) /= 0) error stop
    print*, iand(a3, a1)
    if (iand(a3, a1) /= 5) error stop
    print*, iand(a2, a4)
    if (iand(a2, a4) /= 8) error stop
    print*, iand(a5, a6)
    if (iand(a5, a6) /= -6) error stop
  
end program 