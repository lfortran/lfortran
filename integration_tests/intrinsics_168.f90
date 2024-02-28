program intrinsics_168
    integer :: a1 = 5  
    integer :: a2 = 8
    integer :: a3 = -1
    integer :: a4 = -4
    integer :: a5 = -2
    integer :: a6 = -5
    
    print*, ibclr(5, 8)
    if (ibclr(5, 8) /= 5) error stop
    print*, ibclr(-1, 5)
    if (ibclr(-1, 5) /= -33) error stop

    print*, ibclr(a1, a2)
    if (ibclr(a1, a2) /= 5) error stop
    print*, ibclr(a3, a1)
    if (ibclr(a3, a1) /= -33) error stop
    print*, ibclr(a2, a4)
    if (ibclr(a2, a4) /= 8) error stop
    print*, ibclr(a5, a6)
    if (ibclr(a5, a6) /= -134217730) error stop

end program 