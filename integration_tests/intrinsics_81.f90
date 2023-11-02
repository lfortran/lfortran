program intrinsics_81
    real, allocatable :: C(:,:), C2(:,:)
    integer :: n
    n = 960
    allocate(C(n,n), C2(n,n))

    C = 932.0
    C2 = 320.0

    print *, "Error: ", maxval(abs(C-C2))
    if( maxval(abs(C - C2)) /= 612.00 ) error stop
end program
