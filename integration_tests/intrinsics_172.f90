program intrinsics_172
    integer :: a1 = 3 
    integer :: a2 = 2
    integer :: a3 = 1
    integer :: a4 = -5

    print*, ibits(3, 2, 1)
    if (ibits(3, 2, 1) /= 0) error stop
    print*, ibits(-5, 2, 2)
    if (ibits(-5, 2, 2) /= 2) error stop

    print*, ibits(a1, a2, a3)
    if (ibits(a1, a2, a3) /= 0) error stop
    print*, ibits(a4, a2, a2)
    if (ibits(a4, a2, a2) /= 2) error stop

end program