program intrinsics_166
    integer :: a1 = 5  
    integer :: a2 = 8
    integer :: a3 = -1
    integer :: a4 = -4
    integer :: a5 = -2
    integer :: a6 = -5
  
    print*, ibset(5, 8)
    if (ibset(5, 8) /= 261) error stop
    print*, ibset(-1, 5)
    if (ibset(-1, 5) /= -1) error stop
  
    print*, ibset(a1, a2)
    if (ibset(a1, a2) /= 261) error stop
    print*, ibset(a3, a1)
    if (ibset(a3, a1) /= -1) error stop
    print*, ibset(a2, a4)
    if (ibset(a2, a4) /= 268435464) error stop
    print*, ibset(a5, a6)
    if (ibset(a5, a6) /= -2) error stop
  
end program 
  
