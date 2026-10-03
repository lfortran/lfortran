program intrinsics_448
    implicit none
    character(kind=4), parameter :: ar1(5) = achar([57, 59, 65, 90, 93], kind=4)
    if (any(iachar(ar1, kind=4) /= [57, 59, 65, 90, 93])) error stop
    print *, ar1
end program intrinsics_448